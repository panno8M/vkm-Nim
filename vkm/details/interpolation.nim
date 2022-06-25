import std/strutils
import ../details/normals
import ../math
import ../angles
import ../vectors
import ../quaternions

# Basic //==================================================================== #

type
  Fac* = distinct float32

func fac*(x: float32): Fac = Fac(x)
func percent*(x: float32): Fac = fac x/100

func `'fac`*(x: string): Fac {.compileTime.} = fac x.parseFloat()
func `'percent`*(x: string): Fac {.compileTime.} = percent x.parseFloat()

func `^`*(x: Fac): Fac = fac 1 - x.float32
func clamp*(x: Fac): Fac = fac x.float32.clamp(0, 1)

template `*`[T](x:T, f:Fac): untyped = x * float32(f)
template `*`[T](f:Fac, x:T): untyped = float32(f) * x


# ====================================================================// Basic #
# Lerp //===================================================================== #

# Scalars
func lerp*[T](a, b: T; fac: Fac): T = a * ^fac + b * fac

# Vectors
func lerp*[N; T](a, b: Vec[N, T]; fac: Fac): Vec[N,T] =
  lerp[Vec[N, T]](a, b, fac)
func lerp*[N; T](a, b: Normalized[array[N,T]]; fac: Fac): Normalized[array[N,T]] = normalize do:
  lerp(a.unwrap, b.unwrap, fac)

# Quaternions
func lerp*[T](a, b:  Quat[T]; fac: Fac):  Quat[T] =
  lerp( a.asVec4,  b.asVec4, fac)
func lerp*[T](a, b: NQuat[T]; fac: Fac): NQuat[T] = asNormalized do:
  lerp(a.asNVec4, b.asNVec4, fac)


# =====================================================================// Lerp #
# Slerp //==================================================================== #

# Basic
func slerp[T](a, b: T; theta: Radian32; fac: Fac): T =
  (a * sin(theta * ^fac).fac + b * sin(theta * fac).fac) / sin(theta)

func slerp[T](a, b: T; cosTheta: float32; fac: Fac): T =
  let (bp, cosTheta) =
    # Quaternions have the property that q and -q represent the same rotation.
    when T is (Quat or NQuat):
      if cosTheta < 0: (-b, -cosTheta)
      else:            ( b,  cosTheta)
    else: (b, cosTheta)

  if cosTheta ~= -1: raise DivByZeroDefect.newException( message =
    "The two arguments are on the same line.")
  if cosTheta ~= 1: return a

  slerp(a, bp, arccos(cosTheta), fac)

# Vectors
func slerp*[N; T: SomeFloat](a, b: Vec[N, T]; fac: Fac): Vec[N, T] =
  let cosTheta = dot(a, b) / (a.norm * b.norm)
  slerp(a, b, cosTheta, fac)

func slerp*[N; T](a, b: Normalized[array[N,T]]; fac: Fac): Normalized[array[N,T]] =
  let cosTheta = dot(a.unwrap, b.unwrap)
  asNormalized slerp(a.unwrap, b.unwrap, cosTheta, fac)

# Quaternions
func slerp*[T](a, b:  Quat[T]; fac: Fac):  Quat[T] =
  let cosTheta = dot(a, b) / (a.norm * b.norm)
  slerp(a, b, cosTheta, fac)
func slerp*[T](a, b: NQuat[T]; fac: Fac): NQuat[T] =
  let cosTheta = dot(a.unwrap.vec, b.unwrap.vec)
  slerp(a.unwrap, b.unwrap, cosTheta, fac).asNormalized


# ====================================================================// Slerp #
# Others //=================================================================== #

func smoothstep*[N; T](edge0,edge1,x: Vec[N,T]): Vec[N,T] =
  ## performs smooth Hermite interpolation between 0 and 1 when edge0 < x < edge1.
  ## This is useful in cases where a threshold function with a smooth transition is desired
  # untested
  let t = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0)
  return t * t * (3 - 2 * t)
func smoothstep*[N; T](edge0,edge1: T; x: Vec[N,T]): Vec[N,T] =
  ## performs smooth Hermite interpolation between 0 and 1 when edge0 < x < edge1.
  ## This is useful in cases where a threshold function with a smooth transition is desired
  # untested
  let t = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0)
  return t * t * (3 - 2 * t)

func step*[N; T](edge,x: Vec[N,T]): Vec[N,T] =
  ## For element i of the return value, 0.0 is returned if x[i] < edge[i], and 1.0 is returned otherwise
  for i in 0 ..< N: result[i] = T(x[i] >= edge[i])
func step*[N; T](edge: T; x: Vec[N,T]): Vec[N,T] =
  ## For element i of the return value, 0.0 is returned if x[i] < edge, and 1.0 is returned otherwise
  for i in 0 ..< N: result[i] = T(x[i] >= edge)


# ===================================================================// Others #
# Iterables //================================================================ #

iterator lerp*[T](a,b: T; count: Natural): T =
  let count = count-1
  for cnt in 0..count:
    yield lerp(a, b, (cnt.float32/count.float32).fac)
iterator idxLerp*[T](a,b: T; count: Natural): (int, T) =
  let count = count-1
  for cnt in 0..count:
    yield (cnt, lerp(a, b, (cnt.float32/count.float32).fac))

iterator slerp*[T](a,b: T; count: Natural): T =
  let count = count-1
  for cnt in 0..count:
    yield slerp(a, b, (cnt.float32/count.float32).fac)
iterator idxSlerp*[T](a,b: T; count: Natural): (int, T) =
  let count = count-1
  for cnt in 0..count:
    yield (cnt, slerp(a, b, (cnt.float32/count.float32).fac))

# ================================================================// Iterables #