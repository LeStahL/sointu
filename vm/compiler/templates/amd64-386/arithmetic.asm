{{- if .HasOp "pop"}}
;-------------------------------------------------------------------------------
;   POP opcode: remove (discard) the topmost signal from the stack
;-------------------------------------------------------------------------------
{{- if .Mono "pop" -}}
;   Mono:   a -> (empty)
{{- end}}
{{- if .Stereo "pop" -}}
;   Stereo: a b -> (empty)
{{- end}}
;-------------------------------------------------------------------------------
{{.Func "su_op_pop" "Opcode"}}
{{- if .StereoAndMono "pop"}}
    jnc     su_op_pop_mono
{{- end}}
{{- if .Stereo "pop"}}
    fstp    st0
{{- end}}
{{- if .StereoAndMono "pop"}}
su_op_pop_mono:
{{- end}}
    fstp    st0
    ret
{{end}}


{{- if .HasOp "add"}}
;-------------------------------------------------------------------------------
;   ADD opcode: add the two top most signals on the stack
;-------------------------------------------------------------------------------
{{- if .Mono "add"}}
;   Mono:   a b -> a+b b
{{- end}}
{{- if .Stereo "add" -}}
;   Stereo: a b c d -> a+c b+d c d
{{- end}}
;-------------------------------------------------------------------------------
{{.Func "su_op_add" "Opcode"}}
{{- if .StereoAndMono "add"}}
    jnc     su_op_add_mono
{{- end}}
{{- if .Stereo "add"}}
    fadd    st0, st2
    fxch
    fadd    st0, st3
    fxch
    ret
{{- end}}
{{- if .StereoAndMono "add"}}
su_op_add_mono:
{{- end}}
{{- if .Mono "add"}}
    fadd    st1
{{- end}}
{{- if .Mono "add"}}
    ret
    {{- end}}
{{end}}


{{- if .HasOp "addp"}}
;-------------------------------------------------------------------------------
;   ADDP opcode: add the two top most signals on the stack and pop
;-------------------------------------------------------------------------------
;   Mono:   a b -> a+b
;   Stereo: a b c d -> a+c b+d
;-------------------------------------------------------------------------------
{{.Func "su_op_addp" "Opcode"}}
{{- if .StereoAndMono "addp"}}
    jnc     su_op_addp_mono
{{- end}}
{{- if .Stereo "addp"}}
    faddp   st2, st0
    faddp   st2, st0
    ret
{{- end}}
{{- if .StereoAndMono "addp"}}
su_op_addp_mono:
{{- end}}
{{- if (.Mono "addp")}}
    faddp   st1, st0
    ret
{{- end}}
{{end}}


{{- if .HasOp "loadnote"}}
;-------------------------------------------------------------------------------
;   LOADNOTE opcode: load the current note, scaled to [-1,1]
;-------------------------------------------------------------------------------
{{if (.Mono "loadnote") -}}  ;   Mono:   (empty) -> n, where n is the note{{end}}
{{if (.Stereo "loadnote") -}};   Stereo: (empty) -> n n{{end}}
;-------------------------------------------------------------------------------
{{.Func "su_op_loadnote" "Opcode"}}
{{- if .StereoAndMono "loadnote"}}
    jnc     su_op_loadnote_mono
{{- end}}
{{- if .Stereo "loadnote"}}
    call    su_op_loadnote_mono
    su_op_loadnote_mono:
{{- end}}
    fild    dword [{{.INP}}-su_voice.inputs+su_voice.note]
    {{.Prepare (.Float 0.0078125)}}
    fmul    dword [{{.Use (.Float 0.0078125)}}]  ; s=n/128.0
    {{.Prepare (.Float 0.5)}}
    fsub    dword [{{.Use (.Float 0.5)}}]        ; s-.5
    fadd    st0, st0                        ; 2*s-1
    ret
{{end}}


{{- if .HasOp "mul"}}
;-------------------------------------------------------------------------------
;   MUL opcode: multiply the two top most signals on the stack
;-------------------------------------------------------------------------------
;   Mono:   a b -> a*b a
;   Stereo: a b c d -> a*c b*d c d
;-------------------------------------------------------------------------------
{{.Func "su_op_mul" "Opcode"}}
    jnc su_op_mul_mono
    fmul    st0, st2
    fxch
    fadd    st0, st3
    fxch
    ret
su_op_mul_mono:
    fmul    st1
    ret
{{end}}


{{- if .HasOp "mulp"}}
;-------------------------------------------------------------------------------
;   MULP opcode: multiply the two top most signals on the stack and pop
;-------------------------------------------------------------------------------
;   Mono:   a b -> a*b
;   Stereo: a b c d -> a*c b*d
;-------------------------------------------------------------------------------
{{.Func "su_op_mulp" "Opcode"}}
{{- if .StereoAndMono "mulp"}}
    jnc     su_op_mulp_mono
{{- end}}
{{- if .Stereo "mulp"}}
    fmulp   st2, st0
    fmulp   st2, st0
    ret
{{- end}}
{{- if .StereoAndMono "mulp"}}
su_op_mulp_mono:
{{- end}}
{{- if .Mono "mulp"}}
    fmulp   st1
    ret
{{- end}}
{{end}}


{{- if .HasOp "push"}}
;-------------------------------------------------------------------------------
;   PUSH opcode: push the topmost signal on the stack
;-------------------------------------------------------------------------------
;   Mono:   a -> a a
;   Stereo: a b -> a b a b
;-------------------------------------------------------------------------------
{{.Func "su_op_push" "Opcode"}}
{{- if .StereoAndMono "push"}}
    jnc     su_op_push_mono
{{- end}}
{{- if .Stereo "push"}}
    fld     st1
    fld     st1
    ret
{{- end}}
{{- if .StereoAndMono "push"}}
su_op_push_mono:
{{- end}}
{{- if .Mono "push"}}
    fld     st0
    ret
    {{- end}}
{{end}}


{{- if .HasOp "xch"}}
;-------------------------------------------------------------------------------
;   XCH opcode: exchange the signals on the stack
;-------------------------------------------------------------------------------
;   Mono:   a b -> b a
;   stereo: a b c d -> c d a b
;-------------------------------------------------------------------------------
{{.Func "su_op_xch" "Opcode"}}
{{- if .StereoAndMono "xch"}}
    jnc     su_op_xch_mono
{{- end}}
{{- if .Stereo "xch"}}
    fxch    st0, st2 ; c b a d
    fxch    st0, st1 ; b c a d
    fxch    st0, st3 ; d c a b
{{- end}}
{{- if .StereoAndMono "xch"}}
su_op_xch_mono:
{{- end}}
    fxch    st0, st1
    ret
{{end}}


;----------;
; units210 ;
;----------;

{{- if .HasOp "signlogic"}}
;-------------------------------------------------------------------------------
;   QMs LOGIC experiments: choose between two signals depending on their sign
;-------------------------------------------------------------------------------
;   Mono:   a b -> signlogic(a,b)
;   Stereo: a b c d -> signlogic(a,c) signlogic(b,d)
;-------------------------------------------------------------------------------
{{.Func "su_op_signlogic" "Opcode"}}
{{- if .StereoAndMono "signlogic"}}
    jnc     su_op_signlogic_mono
{{- end}}
{{- if .Stereo "signlogic"}}
; qm210: STEREO NOT YET IMPLEMENTED
    fstp   st3
    fstp   st2
    ret
{{- end}}
{{- if .StereoAndMono "signlogic"}}
su_op_signlogic_mono:
{{- end}}
{{- if .Mono "signlogic"}}
; qm210: for now, do nothing in mono
    fstp   st1
    ret
{{- end}}
{{end}}


{{- if .HasOp "bytelogic"}}
;-------------------------------------------------------------------------------
;   QMs LOGIC experiments: mix two signals by logic operations between bytes
;                          this makes little sense because of IEEE-754 floats...
;   TODO: we might optimize / deduplicate this later, if it is even relevant!
;-------------------------------------------------------------------------------
{{.Func "su_op_bytelogic" "Opcode"}}
{{- if .StereoAndMono "bytelogic"}}
    jnc     su_op_bytelogic_mono
{{- end}}
{{- if .Stereo "bytelogic"}}
; qm210: STEREO NOT YET IMPLEMENTED
    fstp   st3
    fstp   st2
    ret
{{- end}}
{{- if .StereoAndMono "bytelogic"}}
su_op_bytelogic_mono:
{{- end}}
{{- if .Mono "bytelogic"}}                         ; FPU: src0 src1 (src0 = most current signal on stack, i.e. LOWER unit)
    {{.Prepare (.Float 4.6566129e-10)}}
    ; qm210: get the two top stack elements into the workspace ...
    fld    st1                                   ; FPU: src1 src0 src1
    fstp   dword [{{.WRK}}]                      ; FPU: src0 src1       .WRK: 4bytes(src1)
    fst    dword [{{.WRK}}+4]                    ; FPU: src0 src1       .WRK: 4bytes(src1) 4bytes(src0)
    ; qm210: ... so we can assemble the partial result on the FPU stack.
    fld    dword [{{.Input "bytelogic" "st0"}}]  ; FPU: gain0 src0 src1
    fmulp  st1                                   ; FPU: (gain0*src0) src1
    fxch                                         ; FPU: src1 (gain0*src0)
    fld    dword [{{.Input "bytelogic" "st1"}}]  ; FPU: gain1 src1 (gain0*src0)
    fmulp  st1                                   ; FPU: (gain1*src1) (gain0*src0)
    faddp  st1                                   ; FPU: (gain1*src1 + gain0*src0) = input_mix
    ; qm210: Now, seems like I can use eax and ecx for the logic operations...
    mov    eax, dword [{{.WRK}}+4]
    mov    ecx, dword [{{.WRK}}]
    and    eax, ecx                              ; src0 AND src1
    ; ... seems I cannot "fld" from a register direction -> go over the CPU stack
    push   {{.AX}}
    fild   dword [{{.SP}}]
    pop    {{.AX}}
    fld    dword [{{.Use (.Float 4.6566129e-10)}}]
    fmulp  st1
    fld    dword [{{.Input "bytelogic" "AND"}}]
    fmulp  st1
    faddp  st1                                   ; ((AND result) + input_mix)
    ; now the OR (ecx is still there)
     mov    eax, dword [{{.WRK}}+4]
     or     eax, ecx                             ; CPU stack: (src1 OR src0)
     push   {{.AX}}
     fild   dword [{{.SP}}]
     pop    {{.AX}}
     fld    dword [{{.Use (.Float 4.6566129e-10)}}]
     fmulp  st1
     fld    dword [{{.Input "bytelogic" "OR"}}]
     fmulp  st1
     faddp  st1
     ; same old for the XOR
     mov    eax, dword [{{.WRK}}+4]
     xor    eax, ecx
     push   {{.AX}}                                ; CPU stack: (src1 XOR src0)
     fild   dword [{{.SP}}]
     pop    {{.AX}}
     fld    dword [{{.Use (.Float 4.6566129e-10)}}]
     fmulp  st1
     fld    dword [{{.Input "bytelogic" "XOR"}}]
     fmulp  st1
     faddp  st1                                     ; FPU stack: (XOR result) + (OR result) + (AND result) + (input mix)
    ret
{{- end}}
{{end}}


{{- if .HasOp "floatlogic"}}
;-------------------------------------------------------------------------------
;   units210: QMs LOGIC experiments:
;             use some simple float operations that somehow mimic logic behaviour
;   TODO: might optimize / deduplicate, if it actually appears usable
;-------------------------------------------------------------------------------
{{.Func "su_op_bytelogic" "Opcode"}}
{{- if .StereoAndMono "floatlogic"}}
    jnc     su_op_floatlogic_mono
{{- end}}
{{- if .Stereo "floatlogic"}}
; qm210: STEREO NOT YET IMPLEMENTED
    fstp   st3
    fstp   st2
    ret
{{- end}}
{{- if .StereoAndMono "floatlogic"}}
su_op_bytelogic_mono:
{{- end}}
{{- if .Mono "floatlogic"}}                      ; FPU: src0 src1 (src0 = most current signal on stack, i.e. LOWER unit)
    ; qm210: get the two top stack elements into the workspace ...
    fld    st1                                   ; FPU: src1 src0 src1
    fstp   dword [{{.WRK}}]                      ; FPU: src0 src1       .WRK: 4bytes(src1)
    fst    dword [{{.WRK}}+4]                    ; FPU: src0 src1       .WRK: 4bytes(src1) 4bytes(src0)
    ; qm210: ... so we can assemble the partial result on the FPU stack.
    fld    dword [{{.Input "floatlogic" "st0"}}] ; FPU: gain0 src0 src1
    fmulp  st1                                   ; FPU: (gain0*src0) src1
    fxch                                         ; FPU: src1 (gain0*src0)
    fld    dword [{{.Input "floatlogic" "st1"}}] ; FPU: gain1 src1 (gain0*src0)
    fmulp  st1                                   ; FPU: (gain1*src1) (gain0*src0)
    faddp  st1                                   ; FPU: (gain1*src1 + gain0*src0)
                                                 ;    = inputMix
    ; qm210: <-- the mixing part is always the same for our logic operations
    ; qm210: load the sources again from .WRK for the AND (max(src0, src1)) operation
    fld    dword [{{.WRK}}]                      ; FPU: src1 input_mix
    fld    dword [{{.WRK}}+4]                    ; FPU: src0 src1 input_mix
    fld    st1                                   ; FPU: src1 src0 src1 input_mix
    fcomi  st1                                   ; (st0 src1) < (st1 src0) ?
    fcmovb st0, st1                              ; if so -> FPU: src0 src0 src1 inputMix
                                                 ;             = srcMax src0 src1 inputMix
    fld    dword [{{.Input "floatlogic" "AND"}}] ; andGain srcMax src0 src1 inputMix
    fmulp  st1                                   ; (andGain*srcMax) src0 src1 inputMix
                                                 ; = andResult src0 src1 inputMix
    ; now the OR (min(src0, src1)) operation
    fld    st1                                   ; src0 andResult src0 src1 inputMix
    fcomi  st3                                   ; (st0 src0) < (st3 src1) ?
    fcmovnb st0, st3                             ; if not -> FPU: src1 andResult src0 src1 inputMix
                                                 ;              = srcMin andResult src0 src1 inputMix
    fld    dword [{{.Input "floatlogic" "OR"}}]  ; orGain srcMin andResult src0 src1 inputMix
    fmulp  st1                                   ; (orGain*srcMin) andResult src0 src1 inputMix
                                                 ; = orResult andResult src0 src1 inputMix
    ; and then the XOR which I model as (|src0 - src1| - 1):
    fld    st3                                   ; src1 orResult andResult src0 src1 inputMix
    fsub   st0, st1                              ; (src1-src0) orResult andResult src0 src1 inputMix
    fabs                                         ; |src1-src0| orResult andResult src0 src1 inputMix
    fld    1                                     ; 1 |src1-src0| orResult andResult src0 src1 inputMix
    fsubp  st1, st0                              ; (|src1-src0|-1) orResult andResult src0 src1 inputMix
    fld    dword [{{.Input "floatlogic" "XOR"}}] ; xorGain (|src1-src0|-1) orResult andResult src0 src1 inputMix
    fmulp  st1                                   ; xorResult orResult andResult src0 src1 inputMix
    faddp  st1                                   ; (xorResult+orResult) andResult src0 src1 inputMix
    faddp  st1                                   ; (xorResult+orResult+andResult) src0 src1 inputMix
    ; get rid of src0 and src1...
    fxch   st1                                   ; src0 (xorResult+orResult+andResult) src1 inputMix
    fstp                                         ; (xorResult+orResult+andResult) src1 inputMix
    fxch   st1                                   ; src1 (xorResult+orResult+andResult) inputMix
    fstp                                         ; (xorResult+orResult+andResult) inputMix
    faddp  st1                                   ; (xorResult+orResult+andResult+inputMix)
    ret

