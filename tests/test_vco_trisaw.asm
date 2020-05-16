%define BPM 100
%define USE_SECTIONS

%include "../src/sointu.inc"

SU_BEGIN_PATTERNS
    PATTERN 64, 0, 68, 0, 32, 0, 0, 0,  75, 0, 78, 0,   0, 0, 0, 0,
SU_END_PATTERNS

SU_BEGIN_TRACKS
    TRACK   VOICES(1),0
SU_END_TRACKS

SU_BEGIN_PATCH
    SU_BEGIN_INSTRUMENT VOICES(1) ; Instrument0
        SU_ENVELOPE MONO,ATTAC(32),DECAY(32),SUSTAIN(64),RELEASE(64),GAIN(128)
        SU_ENVELOPE MONO,ATTAC(32),DECAY(32),SUSTAIN(64),RELEASE(64),GAIN(128)
        SU_OSCILLAT MONO,TRANSPOSE(64),DETUNE(32),PHASE(0),COLOR(96),SHAPE(64),GAIN(128), FLAGS(TRISAW)
        SU_OSCILLAT MONO,TRANSPOSE(72),DETUNE(64),PHASE(64),COLOR(64),SHAPE(96),GAIN(128), FLAGS(TRISAW)
        SU_MULP     STEREO
        SU_OUT      STEREO,GAIN(128)
    SU_END_INSTRUMENT
SU_END_PATCH

%include "../src/sointu.asm"
