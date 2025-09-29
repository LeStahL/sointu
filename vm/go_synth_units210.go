package vm

import (
	"math"
	"unsafe"
)

// outsources functions from go_synth.go, the units210 opCodes

func scaledEnvelopExponent(value float32) float64 {
	return math.Pow(2, 6*(0.5-float64(value)))
}

func scaledAtan(value float32) float32 {
	return float32(2 / math.Pi * math.Atan(float64(value)))
}

func applySignLogic(valueA, valueB, amountA, amountB, amountAnd, amountOr, amountXor float32) float32 {
	// first implement, think about usefulness later.
	valueAnd := valueA
	if valueA > 0 {
		valueAnd = valueB
	}
	valueOr := valueA
	if valueB > 0 {
		valueOr = valueB
	}
	valueXor := -valueA
	if valueA < 0 && valueB < 0 {
		valueXor = -valueB
	} else if valueA < 0 && valueB > 0 {
		valueXor = valueB
	} else if valueA > 0 && valueB < 0 {
		valueXor = valueA
	}
	return amountA*valueA + amountB*valueB +
		amountAnd*valueAnd +
		amountOr*valueOr +
		amountXor*valueXor
}

func applyByteLogic(valueA, valueB, amountA, amountB, amountAnd, amountOr, amountXor float32) float32 {
	// first implement, think about usefulness later.
	aAddr := (*uint32)(unsafe.Pointer(&valueA))
	bAddr := (*uint32)(unsafe.Pointer(&valueB))
	valueAnd := *aAddr & *bAddr
	valueOr := *aAddr | *bAddr
	valueXor := *aAddr ^ *bAddr
	cAnd := (*int32)(unsafe.Pointer(&valueAnd))
	cOr := (*int32)(unsafe.Pointer(&valueOr))
	cXor := (*int32)(unsafe.Pointer(&valueXor))
	invMaxInt := float32(4.656613e-10)
	outAnd := amountAnd * float32(*cAnd) * invMaxInt
	outOr := amountOr * float32(*cOr) * invMaxInt
	outXor := amountXor * float32(*cXor) * invMaxInt
	inMix := amountA*valueA + amountB*valueB
	return inMix + outAnd + outOr + outXor
}

func applyFloatLogic(valueA, valueB, amountA, amountB, amountAnd, amountOr, amountXor float32) float32 {
	// this might make the most sense of the *Logic opcodes (if any), but we'll see about that.
	valueAnd := min(valueA, valueB)
	valueOr := max(valueA, valueB)
	// this XOR as (|A-B|-1.) is a bit of stretch, but it is what it is
	valueXor := valueA - valueB
	if valueXor < 0 {
		valueXor = -valueXor
	}
	valueXor -= 1.
	return amountA*valueA + amountB*valueB +
		amountAnd*valueAnd + amountOr*valueOr + amountXor*valueXor
}

func NewNepentheneCore(loopSeconds float32, numberEchoes int) nepentheneCore {
	// QM: didn't find an easy accessible source for the constant (sointu always seems to use 44100)
	const sampleRate = 44100.
	// right now, we are using the delayline-buffers for this as well, maybe we should use our own buffers.
	// but as we do use them, the our "work data" have to fit twice in there (original + feedback)
	delayBufferSize := len((delayline{}).buffer)
	maxLoopSeconds := float32(delayBufferSize/2) / sampleRate
	if loopSeconds > maxLoopSeconds {
		loopSeconds = maxLoopSeconds
	}
	spacingSamples := uint32(sampleRate * loopSeconds / float32(numberEchoes))
	loopSamples := spacingSamples * uint32(numberEchoes)
	return nepentheneCore{
		echosets:       make([]nepentheneEchoes, 0),
		echoNumber:     uint32(numberEchoes),
		loopSamples:    loopSamples,
		spacingSamples: spacingSamples,
		sampleRate:     sampleRate,
	}
}

func (n *nepentheneCore) updateEchoes(decayParams []int, state *synthState) {
	for d, decayParam := range decayParams {
		if len(n.echosets) <= d {
			n.echosets = append(n.echosets, nepentheneEchoes{})
		}
		echo := n.echosets[d]
		decay := nonLinearMap(float32(decayParam) / 128.0)
		if echo.currentDecay == decay {
			continue
		}
		echo.currentDecay = decay
		rndFloat := state.rand()
		rndSign := rndFloat >= 0
		if !rndSign {
			rndFloat = -rndFloat
		}
		for p, param := range echo.params {
			param.pos = uint32((float32(p) + rndFloat) * float32(n.spacingSamples))
			param.sign = rndSign
			param.amplitude = 0 // TODO evaluate exp function
		}
	}
}
