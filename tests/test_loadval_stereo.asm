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
        SU_LOADVAL  STEREO,VALUE(40)        
        SU_OUT      STEREO,GAIN(128)
    END_INSTRUMENT
END_PATCH

%include "../src/sointu.asm"