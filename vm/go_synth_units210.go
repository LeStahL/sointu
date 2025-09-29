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
		echoNumber:     numberEchoes,
		loopSamples:    loopSamples,
		bufferSize:     2 * loopSamples,
		spacingSamples: spacingSamples,
		sampleRate:     sampleRate,
	}
}

func (n *nepentheneCore) initializeAll(decayParams []int, state *synthState) {
	// Note: as this is called directly after defining the seed at startup,
	// these turn out always the same. That's nice for development,
	// but might become changeable later on (like echoNumber / bufferSize)
	n.echosets = make([]nepentheneEchoes, len(decayParams))
	for d := range decayParams {
		n.initializeNew(d, decayParams[d], state)
	}
}

func (n *nepentheneCore) initializeNew(index int, decayParam int, state *synthState) {
	n.echosets[index].currentDecayParam = decayParam
	n.echosets[index].params = make([]struct {
		pos       uint32
		amplitude float32
	}, n.echoNumber)
	n.initializeEchoes(index, state)
}

func (n *nepentheneCore) initializeEchoes(index int, state *synthState) {
	echo := &n.echosets[index]
	decayLength := n.decaySamplesFrom(echo.currentDecayParam)
	for p := range echo.params {
		params := &echo.params[p]
		params.amplitude = 1
		rndFloat := state.rand()
		if rndFloat < 0 {
			rndFloat = -rndFloat
			params.amplitude = -1
		}
		samplePos := (float32(p) + rndFloat) * float32(n.spacingSamples)
		params.pos = uint32(samplePos)
		echo.updateDecayAmplitude(p, decayLength)
	}
	echo.normalizationGain = 90.0 / float32(n.echoNumber)
	echo.feedbackGain = decayShape(n.loopSamples, decayLength)
}

func (n *nepentheneCore) updateEchoes(decayParams []int, state *synthState) {
	// The Nepenthene "Velvet Noise" is dependent on the decay time,
	// which is why this parameter is not modulatable
	// (I didn't try, but it's likely way too expensive)
	for d, decayParam := range decayParams {
		if len(n.echosets) == d {
			n.echosets = append(n.echosets, nepentheneEchoes{})
			n.initializeNew(len(n.echosets)-1, decayParam, state)
		}
		echo := &n.echosets[d]
		if echo.currentDecayParam == decayParam {
			continue
		}
		echo.currentDecayParam = decayParam
		decayLength := n.decaySamplesFrom(decayParam)
		for p := range echo.params {
			echo.updateDecayAmplitude(p, decayLength)
		}
		echo.feedbackGain = decayShape(n.loopSamples, decayLength)
	}
}

func (n *nepentheneCore) decaySamplesFrom(param int) float32 {
	return float32(nepentheneTimeFrom(param)) * n.sampleRate
}

func nepentheneTimeFrom(param int) float64 {
	x := float64(1+param) / 256.0
	return 0.5 + 7.5*math.Pow(x, 1./0.7)
}

func (e *nepentheneEchoes) updateDecayAmplitude(index int, decayLength float32) {
	sign := float32(1)
	if e.params[index].amplitude < 0 {
		sign = -1
	}
	decayed := decayShape(e.params[index].pos, decayLength)
	e.params[index].amplitude = sign * decayed * e.normalizationGain
}

func decayShape(samplePosition uint32, decayLength float32) float32 {
	// decay length is "RT60" in number of samples: (1/1000) ^ (pos/decay)
	decayed := float32(samplePosition) / decayLength
	return float32(math.Exp(float64(-3.*decayed) * math.Log(10.)))
}
