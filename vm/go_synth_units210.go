package vm

import (
	"math"
	"unsafe"
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
		amountAnd*valueAnd +
		amountOr*valueOr +
		amountXor*valueXor
}
