when not compiles(SomeFloat):
  type SomeFloat = SomeReal

import vkm/math
import ./details/interpolation

func mod289*[T: SomeFloat](x: T): T = floorMod(x, 289)

func permute*[T: SomeFloat](x: T): T =
  mod289(((x * T(34)) + T(1)) * x)

func taylorInvSqrt*[T: SomeFloat](r: T): T =
  T(1.79284291400159) - T(0.85373472095314) * r

func fade*[T: SomeFloat](t: T): Fac =
  fac (t * t * t) * (t * (t * T(6) - T(15)) + T(10))
