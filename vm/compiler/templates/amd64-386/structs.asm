;-------------------------------------------------------------------------------
;   unit struct
;-------------------------------------------------------------------------------
struc su_unit
    .state      resd    8
    .ports      resd    8
    .size:
endstruc

;-------------------------------------------------------------------------------
;   voice struct
;-------------------------------------------------------------------------------
struc su_voice
    .note       resd    1
    .sustain    resd    1
    .inputs     resd    8
    .reserved   resd    6 ; this is done to so the whole voice is 2^n long, see polyphonic player
    .workspace  resb    63 * su_unit.size
    .size:
endstruc

;-------------------------------------------------------------------------------
;   synthworkspace struct
;-------------------------------------------------------------------------------
struc su_synthworkspace
    .curvoices  resb    32      ; these are used by the multitrack player to store which voice is playing on which track
    .left       resd    1
    .right      resd    1
    .aux        resd    6       ; 3 auxiliary signals
    .voices     resb    32 * su_voice.size
    .size:
endstruc

;-------------------------------------------------------------------------------
;   su_delayline_wrk struct
;-------------------------------------------------------------------------------
struc   su_delayline_wrk
    .dcin       resd    1
    .dcout      resd    1
    .filtstate  resd    1
    .buffer     resd    65536
    .size:
endstruc

;-------------------------------------------------------------------------------
;   su_sample_offset struct
;-------------------------------------------------------------------------------
struc   su_sample_offset  ; length conveniently 8 bytes, so easy to index
    .start      resd    1
    .loopstart  resw    1
    .looplength resw    1
    .size:
endstruc

;-------------------------------------------------------------------------------
; --> units210: su_reverb_core struct
;-------------------------------------------------------------------------------
struc   su_reverb_sampling
    pos         resd    1
    amp         resd    1
    .size:
endstruc

struc   su_reverb_wrk
    ; params will be fixed for now - i.e. decay time can not be modulated
    ; TODO: still, find out how they get in here (compare with the delayline/delaytime structs)
    .params     resb    210 * su_reverb_sampling.size
    .bufferL    resd    131072 ; buffers are 2^17 in max size because 2^16 is too short for 1sec loop length
    .bufferR    resd    131072
    .normgain   resd    1
    .fbgain     resd    1
    .size:
endstruc
;-------------------------------------------------------------------------------
; <-- end of units210
;-------------------------------------------------------------------------------