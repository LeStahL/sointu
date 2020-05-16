;-------------------------------------------------------------------------------
;   OUT Tick
;-------------------------------------------------------------------------------
%if OUT_ID > -1

SECT_TEXT(suopout)

EXPORT MANGLE_FUNC(su_op_out,0) ; l r
    mov     eax, su_synth_obj + su_synth.left
%ifdef INCLUDE_STEREO_OUT
    jnc     su_op_out_mono
    call    su_op_out_mono
    add     eax, 4
su_op_out_mono:
%endif
    fmul    dword [edx+su_out_ports.gain] ; g*l
    fadd    dword [eax]                   ; g*l+o
    fstp    dword [eax]                   ; o'=g*l+o
    ret

%endif ; SU_OUT_ID > -1

;-------------------------------------------------------------------------------
;   Send Tick
;-------------------------------------------------------------------------------
;   Input:      WRK     :   pointer to unit workspace
;               VAL     :   pointer to unit values as bytes
;               ecx     :   pointer to global workspace
;               st0     :   signal
;   Output:     (st0)   :   signal, unless popped
;   Dirty:      eax, edx
;-------------------------------------------------------------------------------
%if SEND_ID > -1

SECT_TEXT(susend)

EXPORT MANGLE_FUNC(su_op_send,0)
    lodsw
%ifdef INCLUDE_STEREO_SEND
    jnc     su_op_send_mono
    mov     edi, eax
    inc     eax  ; send the right channel first
    fxch                        ; r l
    call    su_op_send_mono     ; (r) l
    mov     eax, edi            ; move back to original address
    test    edx, SEND_POP       ; if r was not popped and is still in the stack
    jnz     su_op_send_mono
    fxch                        ; swap them back: l r
su_op_send_mono:
%endif
%ifdef INCLUDE_GLOBAL_SEND
    test    eax, SEND_GLOBAL
    jz      su_op_send_skipglobal
    mov     ecx, su_synth_obj - su_unit.size
su_op_send_skipglobal:
%endif
    test    eax, SEND_POP       ; if the SEND_POP bit is not set
    jnz     su_op_send_skippush
    fld     st0                 ; duplicate the signal on stack: s s
su_op_send_skippush:            ; there is signal s, but maybe also another: s (s)
    fld     dword [edx+su_send_ports.amount]   ; a l (l)
    fsub    dword [c_0_5]                      ; a-.5 l (l)
    fadd    st0                                ; g=2*a-1 l (l)
    and     eax, 0x0000ffff - SEND_POP - SEND_GLOBAL ; eax = send address
    fmulp   st1, st0                           ; g*l (l)
    fadd    dword [ecx+su_unit.size+eax*4]     ; g*l+L (l),where L is the current value
    fstp    dword [ecx+su_unit.size+eax*4]     ; (l)
    ret

%endif ; SU_USE_SEND > -1
