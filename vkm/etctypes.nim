import std/strutils

type
  norm8* = distinct int8
  norm16* = distinct int16

  unorm8* = distinct uint8
  unorm16* = distinct uint16

  scaled8* = distinct int8
  scaled16* = distinct int16

  uscaled8* = distinct uint8
  uscaled16* = distinct uint16

  SomeSignedNorm* = norm8|norm16
  SomeUnsignedNorm* = unorm8|unorm16

  SomeSignedScaled* = scaled8|scaled16
  SomeUnsignedScaled* = uscaled8|uscaled16

  SomeNorm* = SomeSignedNorm|SomeUnsignedNorm
  SomeScaled* = SomeSignedScaled|SomeUnsignedScaled

func normalizeS8*(f: SomeFloat): norm8 =
  if f < -1f: norm8 low(int8)
  elif 1f < f: norm8 high(int8)
  else: norm8 f * int8.high.float

func `==`*(a,b: norm8): bool {.borrow.}
func `high`*(T: typedesc[norm8]): norm8 = norm8 int8.high
func `low`*(T: typedesc[norm8]): norm8 = norm8 int8.low
func `'norm8`*(x: string): norm8 {.compileTime.} = x.parseFloat.normalizeS8
func asFloat*(x: norm8): float = cast[int8](x).float / int8.high.float
func `$`*(x: norm8): string = $x.asFloat

func normalizeU8*(f: SomeFloat): unorm8 =
  if f < 0f: unorm8 low(uint8)
  elif 1f < f: unorm8 high(uint8)
  else: unorm8 f * uint8.high.float

import std/math
func `==`*(a,b: unorm8): bool {.borrow.}
func `~=`*(a,b: unorm8): bool =
  floor((cast[uint8](a).float / uint8.high.float) * 1000) == floor((cast[uint8](b).float / uint8.high.float) * 1000)

func `high`*(T: typedesc[unorm8]): unorm8 = unorm8 uint8.high
func `low`*(T: typedesc[unorm8]): unorm8 = unorm8 uint8.low
func `'unorm8`*(x: string): unorm8 {.compileTime.} = x.parseFloat.normalizeU8
func asFloat*(x: unorm8): float = cast[uint8](x).float / uint8.high.float
func `$`*(x: unorm8): string = $x.asFloat

func `not`*(x: unorm8): unorm8 = unorm8 uint8.high - cast[uint8](x)

when isMainModule:
  import std/unittest
  import std/strformat
  suite "norm":
    test "high":
      check norm8.high.asFloat == 1f
      check unorm8.high.asFloat == 1f
    test "low":
      check norm8.low.asFloat == -1f
      check unorm8.low.asFloat == 0f
    test "safetycast":
      check 16'norm8 == norm8.high
      check -32'norm8 == norm8.low

      check 16'unorm8 == unorm8.high
      check -32'unorm8 == unorm8.low

    # test "flip":
    #   check 0.4'unorm8 == not 0.6'unorm8
  for i in 0..2:
    echo &"{i}: {norm8(i).asFloat}"
  echo ""
  for i in 0..2:
    echo &"{i}: {unorm8(i).asFloat}"