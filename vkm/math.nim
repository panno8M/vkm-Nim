import std/math

export math except
  sin, cos, tan,
  arcsin, arccos, arctan,
  sinh, cosh, tanh,
  arctan2,
  almostEqual

template `~=`*(a, b: untyped): untyped = nearlyEqual(a, b)

template `<$>`*(x, op: untyped): untyped =
  fmap(x, op)
template `>>=`*(x, op: untyped): untyped =
  ## Annoyingly, bind is a reserved word in Nim. So we interpreted
  ## fmap to mean functor map and named it mbind, meaning monad bind.
  mbind(x, op)


func exp2*(x: SomeFloat): SomeFloat {.inline, noinit.} = math.pow(2,x)
func inversesqrt*(x: SomeFloat): SomeFloat {.inline, noinit.} = 1 / sqrt(x)
func fract*(v: SomeFloat): SomeFloat {.inline, noinit.} =
  v - typeof(v) int(v)

func sign*[T](x: T): T {.inline, noinit.} =
  T(x > 0) - T(x < 0)

func floorMod*(x,y: SomeFloat): SomeFloat {.inline, noinit.} =
  ## `floorMod` returns the value of x modulo y. This is computed as x - y * floor(x/y).
  x - y * floor(x / y)

func smoothstep*(edge0,edge1,x: SomeFloat): SomeFloat {.inline, noinit.} =
  ## performs smooth Hermite interpolation between 0 and 1 when edge0 < x < edge1.
  ## This is useful in cases where a threshold function with a smooth transition is desired
  # untested
  let t = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0)
  return t * t * (3 - 2 * t)

func step*[T](edge,x: T): T {.inline, noinit.} =
  return T(x >= edge)

func nearlyEqual*[T: SomeFloat](a, b: T; allowableError: static[T] = 5e-7): bool {.inline.} =
  when allowableError < 0:
    {.error: "allowableError must be more than zero".}
  if a > b: a - b <= allowableError
  else:     b - a <= allowableError