package vm

import (
	"math"
	"unsafe"
)

// outsources functions from go_synth.go, the units210 opCodes

type (
	reverbCore struct {
		echoes         []reverbVoice
		echoNumber     int
		loopSamples    uint32
		bufferSize     uint32
		spacingSamples uint32
		sampleRate     float32
	}

	reverbVoice struct {
		// qm: modeled after https://amalgamatedsignals.com/nepenthe
		//	   might move elsewhere, but I didn't figure a better place
		params []struct {
			pos       uint32
			amplitude float32
		}
		buffers           [][]float32
		currentDecayParam int
		normalizationGain float32
		feedbackGain      float32
	}
)

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

func NewReeeverbCore() reverbCore {
	// QM: didn't find an easy accessible source for the constant (sointu always seems to use 44100)
	const loopSeconds = 1.
	const sampleRate = 44100.
	const numberEchoes = 200
	// right now, we are using the delayline-buffers for this as well, maybe we should use our own buffers.
	// but as we do use them, the our "work data" have to fit twice in there (original + feedback)
	// WIP: just getting rid of the coupling to the delaylines
	//delayBufferSize := len((delayline{}).buffer)
	//maxLoopSeconds := float32(delayBufferSize/2) / sampleRate
	//if loopSeconds > maxLoopSeconds {
	//	loopSeconds = maxLoopSeconds
	//}
	spacingSamples := uint32(sampleRate * loopSeconds / float32(numberEchoes))
	loopSamples := spacingSamples * uint32(numberEchoes)
	return reverbCore{
		echoes:         make([]reverbVoice, 0),
		echoNumber:     numberEchoes,
		loopSamples:    loopSamples,
		bufferSize:     2 * loopSamples,
		spacingSamples: spacingSamples,
		sampleRate:     sampleRate,
	}
}

func (n *reverbCore) initializeAll(interlacedNeeds []int, state *synthState) {
	// Note: as this is called directly after defining the seed at startup,
	// these turn out always the same. That's nice for development,
	// but might become changeable later on (like echoNumber / bufferSize)
	n.echoes = make([]reverbVoice, len(interlacedNeeds)/2)
	for i := 0; i < len(n.echoes); i++ {
		n.initializeNew(i, interlacedNeeds[2*i], interlacedNeeds[2*i+1], state)
	}
}

func (n *reverbCore) initializeNew(index int, decayParam int, nBuffers int, state *synthState) {
	echo := &n.echoes[index]
	echo.currentDecayParam = decayParam
	echo.params = make([]struct {
		pos       uint32
		amplitude float32
	}, n.echoNumber)
	echo.buffers = make([][]float32, nBuffers)
	n.initializeEchoes(index, state)
}

func (n *reverbCore) initializeEchoes(index int, state *synthState) {
	echo := &n.echoes[index]
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
	for b := range echo.buffers {
		echo.buffers[b] = make([]float32, n.bufferSize)
	}
}

func (n *reverbCore) updateEchoes(interlacedNeeds []int, state *synthState) {
	// The Reeeverb "Velvet Noise" is dependent on the decay time,
	// which is why this parameter is not modulatable
	// (I didn't try, but it's likely way too expensive)
	for i := 0; i < len(interlacedNeeds)/2; i++ {
		decayParam := interlacedNeeds[2*i]
		nBuffers := interlacedNeeds[2*i+1]
		if len(n.echoes) == i {
			n.echoes = append(n.echoes, reverbVoice{})
			n.initializeNew(len(n.echoes)-1, decayParam, nBuffers, state)
		}
		echo := &n.echoes[i]
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

func (n *reverbCore) decaySamplesFrom(param int) float32 {
	return float32(reeeverbTimeFrom(param)) * n.sampleRate
}

func reeeverbTimeFrom(param int) float64 {
	x := float64(1+param) / 256.0
	return 0.5 + 7.5*math.Pow(x, 1./0.7)
}

func (e *reverbVoice) updateDecayAmplitude(index int, decayLength float32) {
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
