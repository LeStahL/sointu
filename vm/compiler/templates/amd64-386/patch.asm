;-------------------------------------------------------------------------------
;   su_run_vm function: runs the entire virtual machine once, creating 1 sample
;-------------------------------------------------------------------------------
;   Input:      su_synth_obj.left   :   Set to 0 before calling
;               su_synth_obj.right  :   Set to 0 before calling
;               _CX                 :   Pointer to delay workspace (if needed)
;               _DX                 :   Pointer to synth object
;               COM                 :   Pointer to opcode stream
;               VAL                 :   Pointer to operand stream
;               WRK                 :   Pointer to the last workspace processed
;   Output:     su_synth_obj.left   :   left sample
;               su_synth_obj.right  :   right sample
;   Dirty:      everything
;-------------------------------------------------------------------------------
{{.Func "su_run_vm"}}
    {{- .PushRegs .CX "DelayWorkSpace" .DX "Synth" .COM "OpcodeStream" .WRK "Voice" .VAL "OperandStream" | indent 4}}
    {{- if .RowSync}}
    fild    dword [{{.Stack "Sample"}}]
    {{.Int .Song.SamplesPerRow | .Prepare | indent 8}}
    fidiv   dword [{{.Int .Song.SamplesPerRow | .Use}}]
    fiadd   dword [{{.Stack "Row"}}]
    {{.Call "su_op_sync"}}
    fstp    st0
    {{- end}}
su_run_vm_loop:                                     ; loop until all voices done
    movzx   edi, byte [{{.COM}}]                         ; edi = command byte
    inc     {{.COM}}                                     ; move to next instruction
    add     {{.WRK}}, su_unit.size                       ; move WRK to next unit
    shr     edi, 1                                  ; shift out the LSB bit = stereo bit
    je      su_run_vm_advance                ; the opcode is zero, jump to advance
    mov     {{.INP}}, [{{.Stack "Voice"}}]         ; reset INP to point to the inputs part of voice
    pushf                                          ; push flags to save carry = stereo bit
    add     {{.INP}}, su_voice.inputs
    xor     ecx, ecx                                ; counter = 0
    xor     eax, eax                                ; clear out high bits of eax, as lodsb only sets al
su_transform_operands_loop:
    {{- .Prepare "su_vm_transformcounts-1" | indent 4}}
    cmp     cl, byte [{{.Use "su_vm_transformcounts-1"}}+{{.DI}}]   ; compare the counter to the value in the param count table
    je      su_transform_operands_out
    lodsb                                           ; load the operand from VAL stream
    push    {{.AX}}                                     ; push it to memory so FPU can read it
    fild    dword [{{.SP}}]                             ; load the operand value to FPU stack
    {{- .Prepare (.Float 0.0078125) | indent 4}}
    fmul    dword [{{.Use (.Float 0.0078125)}}]          ; divide it by 128 (0 => 0, 128 => 1.0)
    fadd    dword [{{.WRK}}+su_unit.ports+{{.CX}}*4]         ; add the modulations in the current workspace
    fstp    dword [{{.INP}}+{{.CX}}*4]                       ; store the modulated value in the inputs section of voice
    xor     eax, eax
    mov     dword [{{.WRK}}+su_unit.ports+{{.CX}}*4], eax    ; clear out the modulation ports
    pop     {{.AX}}
    inc     ecx
    jmp     su_transform_operands_loop
su_transform_operands_out:
    popf                                          ; pop flags for the carry bit = stereo bit
    {{- .SaveStack "Opcode"}}
    {{- $x := printf "su_vm_jumptable-%v" .PTRSIZE}}
    {{- .Prepare $x | indent 4}}
    call    [{{.Use $x}}+{{.DI}}*{{.PTRSIZE}}]       ; call the function corresponding to the instruction
    jmp     su_run_vm_loop
su_run_vm_advance:
    {{- if .SupportsPolyphony}}
    mov     {{.WRK}}, [{{.Stack "Voice"}}]         ; WRK points to start of current voice
    add     {{.WRK}}, su_voice.size              ; move to next voice
    mov     [{{.Stack "Voice"}}], {{.WRK}}         ; update the pointer in the stack to point to the new voice
    mov     ecx, [{{.Stack "VoicesRemain"}}]     ; ecx = how many voices remain to process
    dec     ecx                             ; decrement number of voices to process
    bt      dword [{{.Stack "PolyphonyBitmask"}}], ecx ; if voice bit of su_polyphonism not set
    jnc     su_op_advance_next_instrument   ; goto next_instrument
    mov     {{.VAL}}, [{{.Stack "OperandStream"}}] ; if it was set, then repeat the opcodes for the current voice
    mov     {{.COM}}, [{{.Stack "OpcodeStream"}}]
su_op_advance_next_instrument:
    mov     [{{.Stack "OperandStream"}}], {{.VAL}} ; save current VAL as a checkpoint
    mov     [{{.Stack "OpcodeStream"}}], {{.COM}} ; save current COM as a checkpoint
su_op_advance_finish:
    mov     [{{.Stack "VoicesRemain"}}], ecx
    jne     su_run_vm_loop  ; ZF was set by dec ecx
    {{- else}}
    mov     {{.WRK}}, {{.PTRWORD}} [{{.Stack "Voice"}}] ; load pointer to voice to register
    add     {{.WRK}}, su_voice.size              ; shift it to point to following voice
    mov     {{.PTRWORD}} [{{.Stack "Voice"}}], {{.WRK}} ; save back to stack
    dec     dword [{{.Stack "VoicesRemain"}}]  ; voices--
    jne     su_run_vm_loop                          ;   if there's more voices to process, goto vm_loop
    {{- end}}
    {{- .PopRegs .CX .DX .COM .WRK .VAL | indent 4}}
    ret

{{- template "arithmetic.asm" .}}
{{- template "effects.asm" .}}
{{- template "flowcontrol.asm" .}}
{{- template "sinks.asm" .}}
{{- template "sources.asm" .}}
{{- template "gmdls.asm" .}}

{{- if .HasCall "su_nonlinear_map"}}
;-------------------------------------------------------------------------------
;   su_nonlinear_map function: returns 2^(-24*x) of parameter number _AX
;-------------------------------------------------------------------------------
;   Input:      _AX     :   parameter number (e.g. for envelope: 0 = attac, 1 = decay...)
;               INP     :   pointer to transformed operands
;   Output:     st0     :   2^(-24*x), where x is the parameter in the range 0-1
;-------------------------------------------------------------------------------
{{.Func "su_nonlinear_map"}}
    fld     dword [{{.INP}}+{{.AX}}*4]   ; x, where x is the parameter in the range 0-1
    {{.Prepare (.Int 24)}}
    fimul   dword [{{.Use (.Int 24)}}]      ; 24*x
    fchs                        ; -24*x

{{end}}

{{- if or (.HasCall "su_power") (.HasCall "su_nonlinear_map")}}
;-------------------------------------------------------------------------------
;   su_power function: computes 2^x
;-------------------------------------------------------------------------------
;   Input:      st0     :   x
;   Output:     st0     :   2^x
;-------------------------------------------------------------------------------
{{- if not (.HasCall "su_nonlinear_map")}}{{.SectText "su_power"}}{{end}}
{{.Export "su_pow" 0}}
su_power:
    fld1          ; 1 x
    fld st1       ; x 1 x
    fprem         ; mod(x,1) 1 x
    f2xm1         ; 2^mod(x,1)-1 1 x
    faddp st1,st0 ; 2^mod(x,1) x
    fscale        ; 2^mod(x,1)*2^trunc(x) x
                  ; Equal to:
                  ; 2^x x
    fstp st1      ; 2^x
    ret

{{end}}

{{- if .HasOp "distort"}}
;-------------------------------------------------------------------------------
;   DISTORT opcode: apply distortion on the signal
;-------------------------------------------------------------------------------
;   Mono:   x   ->  x*a/(1-a+(2*a-1)*abs(x))            where x is clamped first
;   Stereo: l r ->  l*a/(1-a+(2*a-1)*abs(l)) r*a/(1-a+(2*a-1)*abs(r))
;   This is placed here to be able to flow into waveshaper & also include
;   wave shaper if needed by some other function; need to investigate the
;   best way to do this
;-------------------------------------------------------------------------------
{{.Func "su_op_distort" "Opcode"}}
{{- if .Stereo "distort" -}}
    {{.Call "su_effects_stereohelper"}}
{{- end}}
    fld     dword [{{.Input "distort" "drive"}}]
{{end}}

{{- if or (.HasCall "su_waveshaper") (.HasOp "distort")}}
{{- if .HasOp "distort"}}
su_waveshaper:
{{- else}}
{{.Func "su_waveshaper"}}
{{- end}}
    fld     st0                             ; a a x
    {{.Prepare (.Float 0.5)}}
    fsub    dword [{{.Use (.Float 0.5)}}]                 ; a-.5 a x
    fadd    st0                             ; 2*a-1 a x
    fld     st2                             ; x 2*a-1 a x
    fabs                                    ; abs(x) 2*a-1 a x
    fmulp   st1                             ; (2*a-1)*abs(x) a x
    fld1                                    ; 1 (2*a-1)*abs(x) a x
    faddp   st1                             ; 1+(2*a-1)*abs(x) a x
    fsub    st1                             ; 1-a+(2*a-1)*abs(x) a x
    fdivp   st1, st0                        ; a/(1-a+(2*a-1)*abs(x)) x
    fmulp   st1                             ; x*a/(1-a+(2*a-1)*abs(x))
    ret
{{end}}

{{- if .HasCall "su_effects_stereohelper" }}
;-------------------------------------------------------------------------------
;   su_effects_stereohelper: moves the workspace to next, does the filtering for
;   right channel (pulling the calling address from stack), rewinds the
;   workspace and returns
;-------------------------------------------------------------------------------
{{.Func "su_effects_stereohelper"}}
    jnc     su_effects_stereohelper_mono ; carry is still the stereo bit
    add     {{.WRK}}, 16
    fxch                  ; r l
    call    [{{.SP}}]         ; call whoever called me...
    fxch                  ; l r
    sub     {{.WRK}}, 16       ; move WRK back to where it was
su_effects_stereohelper_mono:
    ret                   ; return to process l/mono sound
{{end}}


{{- if .HasCall "su_effects_reducingstereohelper" }}
;-------------------------------------------------------------------------------
; -- QM-WIP TODO for the logic units in stereo -- does not work yet !!
; -- probably the FPU stack in the functions can only has max 5 elements then!
; -- fix when stereo is actually needed...
;
;   su_effects_doublestereohelper: similar to su_effects_reducing,
;   but for functions that reduce the stack by one, i.e.
;   incoming: (l1 r1 l2 r2)
;   -> move -> (r2 r1 l2 l1) -> (r1 r2 l2 l1)
;   -> run caller -> (r l2 l1)
;   -> move -> (l1 r2 r)
;   -> run caller -> (l r)
;-------------------------------------------------------------------------------
{{.Func "su_effects_reducingstereohelper"}}
    jnc     su_effects_reducingstereohelper_mono ; carry is still the stereo bit
    add     {{.WRK}}, 16
    fxch    st3             ; r2 r1 l2 l1
    fxch    st1             ; r1 r2 l2 l1
    call    [{{.SP}}]       ; call whoever called me... -> r l2 l1
    fxch    st2             ; l1 l2 r
    call    [{{.SP}}]       ; call whoever called me... -> l r
    sub     {{.WRK}}, 16       ; move WRK back to where it was
su_effects_reducingstereohelper_mono:
    ret                   ; return to process l/mono sound
{{end}}


{{- if .HasCall "su_clip"}}
{{.Func "su_clip"}}
    fld1                                    ; 1 x a
    fucomi  st1                             ; if (1 <= x)
    jbe     short su_clip_do                ;   goto Clip_Do
    fchs                                    ; -1 x a
    fucomi  st1                             ; if (-1 < x)
    fcmovb  st0, st1                        ;   x x a
su_clip_do:
    fstp    st1                             ; x' a, where x' = clamp(x)
    ret
{{end}}

;-------------------------------------------------------------------------------
; The opcode table jump table. This is constructed to only include the opcodes
; that are used so that the jump table is as small as possible.
;-------------------------------------------------------------------------------
{{.Data "su_vm_jumptable"}}
{{- range .Instructions}}
    {{$.DPTR}}    su_op_{{.}}
{{- end}}

;-------------------------------------------------------------------------------
; The number of transformed parameters each opcode takes
;-------------------------------------------------------------------------------
{{.Data "su_vm_transformcounts"}}
{{- range .Instructions}}
    db    {{$.TransformCount .}}
{{- end}}
