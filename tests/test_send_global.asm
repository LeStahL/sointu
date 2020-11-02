%define BPM 100

%include "sointu/header.inc"

BEGIN_PATTERNS
    PATTERN 64, HLD, HLD, HLD, HLD, HLD, HLD, HLD,  0, 0, 0, 0, 0, 0, 0, 0
END_PATTERNS

BEGIN_TRACKS
    TRACK VOICES(1),0
END_TRACKS

BEGIN_PATCH
    BEGIN_INSTRUMENT VOICES(1) ; Instrument0
        SU_LOADVAL MONO,VALUE(96)
        SU_SEND     MONO,AMOUNT(96),GLOBALPORT(1,3,0) + SEND_POP
        SU_LOADVAL MONO,VALUE(64)
        SU_LOADVAL MONO,VALUE(64)
        SU_OUT      STEREO,GAIN(128)
    END_INSTRUMENT
    BEGIN_INSTRUMENT VOICES(1)  ; Instrument1
        SU_LOADVAL MONO,VALUE(32)
        SU_SEND     MONO,AMOUNT(96),GLOBALPORT(0,2,0) + SEND_POP
        SU_LOADVAL MONO,VALUE(64)
        SU_LOADVAL MONO,VALUE(64)
        SU_OUT      STEREO,GAIN(128)
    END_INSTRUMENT
END_PATCH

%include "sointu/footer.inc"
