package vm

import (
	"math"
	"unsafe"
)

func processUnits210(stack []float32, unit *unit, opCode byte, stereo bool, params [8]float32, voices []voice) []float32 {
	l := len(stack)
	switch opCode {

	case opEnvelopexp:
		if !voices[0].sustain {
			unit.state[0] = envStateRelease // set state to release
		}
		state := unit.state[0]
		level := unit.state[1]
		exponent := float64(1)
		baseline := float32(0)
		switch state {
		case envStateAttack:
			exponent = scaledEnvelopExponent(params[1])
			level += nonLinearMap(params[0])
			if level >= 1 {
				level = 1
				state = envStateDecay
			}
		case envStateDecay:
			exponent = scaledEnvelopExponent(params[3])
			sustain := params[4]
			baseline = sustain
			level -= nonLinearMap(params[2])
			if level <= sustain {
				level = sustain
			}
		case envStateRelease:
			level -= nonLinearMap(params[5])
			if level <= 0 {
				level = 0
			}
		}
		unit.state[0] = state
		unit.state[1] = level
		expLevel := float32(math.Pow(float64(level), exponent))
		output := (baseline + (1-baseline)*expLevel) * params[6]
		stack = append(stack, output)
		if stereo {
			stack = append(stack, output)
		}

	case opAtan:
		if stereo {
			stack[l-2] = scaledAtan(stack[l-2])
		}
		stack[l-1] = scaledAtan(stack[l-1])

	case opSignlogic:
		if stereo {
			stack[l-3] = applySignLogic(stack[l-1], stack[l-3], params[0], params[1], params[2], params[3], params[4])
			stack[l-4] = applySignLogic(stack[l-2], stack[l-4], params[0], params[1], params[2], params[3], params[4])
			stack = stack[:l-2]
		} else {
			stack[l-2] = applySignLogic(stack[l-1], stack[l-2], params[0], params[1], params[2], params[3], params[4])
			stack = stack[:l-1]
		}

	case opIllogic:
		if stereo {
			stack[l-3] = applyIllogic(stack[l-1], stack[l-3], params[0], params[1], params[2], params[3], params[4])
			stack[l-4] = applyIllogic(stack[l-2], stack[l-4], params[0], params[1], params[2], params[3], params[4])
			stack = stack[:l-2]
		} else {
			stack[l-2] = applyIllogic(stack[l-1], stack[l-2], params[0], params[1], params[2], params[3], params[4])
			stack = stack[:l-1]
		}
	}

	return stack
}

func scaledEnvelopExponent(value float32) float64 {
	return math.Pow(2, 6*(0.5-float64(value)))
}

func scaledAtan(value float32) float32 {
	return float32(2 / math.Pi * math.Atan(float64(value)))
}

func applySignLogic(valueA, valueB, amountA, amountB, amountAnd, amountOr, amountXor float32) float32 {
	// first implement, think about usefulness later.
	if valueA == 0 || valueB == 0 {
		return 0
	}
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

func applyIllogic(valueA, valueB, amountA, amountB, amountAnd, amountOr, amountXor float32) float32 {
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
