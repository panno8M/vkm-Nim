from std/strutils import parseFloat
import vkm/math
import std/math as stdmath

# Type //===================================================================== #

type
  Degree*[T: SomeFloat] = distinct T
  Radian*[T: SomeFloat] = distinct T
  Degree32* = Degree[float32]
  Degree64* = Degree[float64]
  Radian32* = Radian[float32]
  Radian64* = Radian[float64]

  SomeDegree* = Degree32 | Degree64
  SomeRadian* = Radian32 | Radian64
  SomeAngle* = SomeDegree | SomeRadian


# =====================================================================// Type #
# Constructor //============================================================== #

{.push, inline.}
func rad*[T: SomeFloat](x: T): Radian[T] = Radian[T] x
func deg*[T: SomeFloat](x: T): Degree[T] = Degree[T] x
func rad32*[T: SomeFloat](x: T): Radian32 = Radian32 x
func deg32*[T: SomeFloat](x: T): Degree32 = Degree32 x
func rad64*[T: SomeFloat](x: T): Radian64 = Radian64 x
func deg64*[T: SomeFloat](x: T): Degree64 = Degree64 x

func `'rad`*(x: string): Radian64 = Radian64 x.parseFloat
func `'deg`*(x: string): Degree64 = Degree64 x.parseFloat
func `'rad64`*(x: string): Radian64 = Radian64 x.parseFloat
func `'deg64`*(x: string): Degree64 = Degree64 x.parseFloat
func `'rad32`*(x: string): Radian32 = Radian32 x.parseFloat
func `'deg32`*(x: string): Degree32 = Degree32 x.parseFloat
{.pop.}


# ==============================================================// Constructor #
# Converter //================================================================ #

{.push, inline.}
func rad*[T: SomeFloat](x: Degree[T]): Radian[T] = Radian[T] T(x) * PI / 180
func deg*[T: SomeFloat](x: Radian[T]): Degree[T] = Degree[T] T(x) * 180 / PI

func to64*(x: Radian32): Radian64 = Radian64 x
func to64*(x: Degree32): Degree64 = Degree64 x

func rad32*[T: SomeFloat](x: Radian[T]): Radian32 = Radian32 x
func deg32*[T: SomeFloat](x: Degree[T]): Degree32 = Degree32 x
func rad32*[T: SomeFloat](x: Degree[T]): Radian32 = Radian32 x.rad
func deg32*[T: SomeFloat](x: Radian[T]): Degree32 = Degree32 x.deg
{.pop.}


# ================================================================// Converter #
# Stringify //================================================================ #

{.push, inline.}
func `$`*[T: SomeFloat](x: Degree[T]): string = $T(x) & (
  when defined(noUnicode) or defined(windows): " deg"
  else: "°")
func `$`*[T: SomeFloat](x: Radian[T]): string = $T(x) & " rad"
{.pop.}


# ================================================================// Stringify #
# Basic Math //=============================================================== #

template defop(Angle: typedesc): untyped =
  {.push, borrow.}
  func `+`*(a, b: Angle[float]): Angle[float]
  func `-`*(a, b: Angle[float]): Angle[float]
  func `*`*(a, b: Angle[float]): Angle[float]
  func `/`*(a, b: Angle[float]): Angle[float]
  func `+`*(a: Angle[float]; b: float): Angle[float]
  func `-`*(a: Angle[float]; b: float): Angle[float]
  func `*`*(a: Angle[float]; b: float): Angle[float]
  func `/`*(a: Angle[float]; b: float): Angle[float]
  func `+`*(a: float; b: Angle[float]): Angle[float]
  func `-`*(a: float; b: Angle[float]): Angle[float]
  func `*`*(a: float; b: Angle[float]): Angle[float]
  func `/`*(a: float; b: Angle[float]): Angle[float]

  func `-`*(x: Angle[float]): Angle[float]

  func `==`*(a, b: Angle[float]): bool
  func `<`*(a, b: Angle[float]): bool
  func `<=`*(a, b: Angle[float]): bool

  func `+`*(a, b: Angle[float32]): Angle[float32]
  func `-`*(a, b: Angle[float32]): Angle[float32]
  func `*`*(a, b: Angle[float32]): Angle[float32]
  func `/`*(a, b: Angle[float32]): Angle[float32]
  func `+`*(a: Angle[float32]; b: float32): Angle[float32]
  func `-`*(a: Angle[float32]; b: float32): Angle[float32]
  func `*`*(a: Angle[float32]; b: float32): Angle[float32]
  func `/`*(a: Angle[float32]; b: float32): Angle[float32]
  func `+`*(a: float32; b: Angle[float32]): Angle[float32]
  func `-`*(a: float32; b: Angle[float32]): Angle[float32]
  func `*`*(a: float32; b: Angle[float32]): Angle[float32]
  func `/`*(a: float32; b: Angle[float32]): Angle[float32]

  func `-`*(x: Angle[float32]): Angle[float32]

  func `==`*(a, b: Angle[float32]): bool
  func `<`*(a, b: Angle[float32]): bool
  func `<=`*(a, b: Angle[float32]): bool
  {.pop.}

  {.push, inline.}
  func nearlyEqual*(a, b: Angle[float];
        allowableError: static float = 5e-7): bool =
    nearlyEqual(float(a), float(b), allowableError)

  func nearlyEqual*(a, b: Angle[float32];
        allowableError: static float32 = 5e-7): bool =
    nearlyEqual(float32(a), float32(b), allowableError)
  {.pop.}


defop Degree
defop Radian

{.push, inline.}
func `==`*[T: SomeFloat](a: Radian[T]; b: Degree[T]): bool = a == b.rad
func `<=`*[T: SomeFloat](a: Radian[T]; b: Degree[T]): bool = a <= b.rad
func `<` *[T: SomeFloat](a: Radian[T]; b: Degree[T]): bool = a < b.rad
func nearlyEqual*[T: SomeFloat](a: Radian[T]; b: Degree[T];
      allowableError: static T = 5e-7): bool =
  nearlyEqual(a, b.rad, allowableError)
{.pop.}

template `==`*[T: SomeFloat](a: Degree[T]; b: Radian[T]): bool = b == a
template `<=`*[T: SomeFloat](a: Degree[T]; b: Radian[T]): bool = b <= a
template `<` *[T: SomeFloat](a: Degree[T]; b: Radian[T]): bool = b < a
template nearlyEqual*[T: SomeFloat](a: Degree[T]; b: Radian[T];
      allowableError: static T = 5e-7): bool =
  nearlyEqual(b, a, allowableError)


# ===============================================================// Basic Math #
# Trigonometric //============================================================ #

{.push, inline.}
func sin*[T: SomeFloat](x: Radian[T]): T = stdmath.sin T(x)
func cos*[T: SomeFloat](x: Radian[T]): T = stdmath.cos T(x)
func tan*[T: SomeFloat](x: Radian[T]): T = stdmath.tan T(x)

func sinh*[T: SomeFloat](x: Radian[T]): T = stdmath.sinh T(x)
func cosh*[T: SomeFloat](x: Radian[T]): T = stdmath.cosh T(x)
func tanh*[T: SomeFloat](x: Radian[T]): T = stdmath.tanh T(x)

func arcsin*[T: SomeFloat](x: T): Radian[T] = Radian[T] stdmath.arcsin x
func arccos*[T: SomeFloat](x: T): Radian[T] = Radian[T] stdmath.arccos x
func arctan*[T: SomeFloat](x: T): Radian[T] = Radian[T] stdmath.arctan x
func arctan2*[T: SomeFloat](y, x: T): Radian[T] = Radian[T] stdmath.arctan2(y, x)

func sin*[T: SomeFloat](x: Degree[T]): T = stdmath.sin T(x.rad)
func cos*[T: SomeFloat](x: Degree[T]): T = stdmath.cos T(x.rad)
func tan*[T: SomeFloat](x: Degree[T]): T = stdmath.tan T(x.rad)

func sinh*[T: SomeFloat](x: Degree[T]): T = stdmath.sinh T(x.rad)
func cosh*[T: SomeFloat](x: Degree[T]): T = stdmath.cosh T(x.rad)
func tanh*[T: SomeFloat](x: Degree[T]): T = stdmath.tanh T(x.rad)
{.pop.}


# ============================================================// Trigonometric #
