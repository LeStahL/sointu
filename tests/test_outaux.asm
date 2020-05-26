%define BPM 100

%include "../src/sointu.inc"

BEGIN_PATTERNS
    PATTERN 64, HLD, HLD, HLD, HLD, HLD, HLD, HLD,  0, 0, 0, 0, 0, 0, 0, 0
END_PATTERNS

BEGIN_TRACKS
    TRACK VOICES(1),0
END_TRACKS

BEGIN_PATCH
    BEGIN_INSTRUMENT VOICES(1) ; Instrument0
        SU_LOADVAL  MONO,VALUE(0)
        SU_OUTAUX   MONO,OUTGAIN(32),AUXGAIN(64)
        SU_IN       MONO,CHANNEL(0)
        SU_IN       MONO,CHANNEL(2)
        SU_LOADVAL  MONO,VALUE(48)
        SU_LOADVAL  MONO,VALUE(128)
        SU_ADDP     STEREO
        SU_OUT      STEREO,GAIN(128)
    END_INSTRUMENT
END_PATCH

%include "../src/sointu.asm"