%define BPM 100
%define USE_SECTIONS

%include "../src/sointu.inc"

SU_BEGIN_PATTERNS
    PATTERN 64, HLD, HLD, HLD, HLD, HLD, HLD, HLD,  0, 0, 0, 0, 0, 0, 0, 0
SU_END_PATTERNS

SU_BEGIN_TRACKS
    TRACK VOICES(1),0
SU_END_TRACKS

SU_BEGIN_PATCH
    SU_BEGIN_INSTRUMENT VOICES(1) ; Instrument0
        SU_ENVELOPE MONO,ATTAC(64),DECAY(64),SUSTAIN(64),RELEASE(80),GAIN(128)
        SU_DISTORT  MONO,DRIVE(32)
        SU_ENVELOPE MONO,ATTAC(64),DECAY(64),SUSTAIN(64),RELEASE(80),GAIN(128)
        SU_DISTORT  MONO,DRIVE(96)
        SU_OUT      STEREO,GAIN(128)
    SU_END_INSTRUMENT
SU_END_PATCH

%include "../src/sointu.asm"
