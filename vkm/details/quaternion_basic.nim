import ./normals
import ../functionalops
import ../angles
import ../math
import ../vectors

# Type //===================================================================== #

type Quat*[T: SomeFloat] = object
  vec*: array[4,T]


# =====================================================================// Type #
# Constructor //============================================================== #

func quat*[T](vec: array[4,T]): Quat[T] = Quat[T](vec: vec)
func quat*[T](dirFrom, dirTo: sink array[3,T]): Quat[T] =
  quat vec(cross(dirFrom, dirTo), T(1) + dot(dirFrom, dirTo))


# ==============================================================// Constructor #
# Utility //================================================================== #

func `$`*[T](q : Quat[T]) : string =
  "quat " & $q.vec

template norm2*[T](q:Quat[T]): T = q.vec.norm2
template norm*[T](q:Quat[T]): T = q.vec.norm

func conjugate*[T](q: Quat[T]): Quat[T] = quat [-q.vec[0], -q.vec[1], -q.vec[2], q.vec[3]]
func inverse  *[T](q: Quat[T]): Quat[T] = q.conjugate / q.vec.norm2

template `==`*[T](q1, q2:  Quat[T]): bool = q1.vec == q2.vec
template nearlyEqual*[T](q1, q2: Quat[T]; allowableError: static T = 5e-7): array[4, bool] =
  nearlyEqual(q1.vec, q2.vec, allowableError)

# ==================================================================// Utility #
# Math //===================================================================== #
template shortaccess(q): untyped =
  template `q x`: untyped = q.vec.x
  template `q y`: untyped = q.vec.y
  template `q z`: untyped = q.vec.z
  template `q w`: untyped = q.vec.w

func `*`*[T](a,b: Quat[T]): Quat[T] =
  shortaccess a
  shortaccess b
  quat [
    aw*bx + ax*bw + ay*bz - az*by,
    aw*by + ay*bw + az*bx - ax*bz,
    aw*bz + az*bw + ax*by - ay*bx,
    aw*bw - ax*bx - ay*by - az*bz ]


func `*`*[T](q: Quat[T]; s: T) : Quat[T] =
  for i in 0..3: result.vec[i] = q.vec[i] * s
template `*`*[T](s: T; q : Quat[T]) : Quat[T] = q * s

func `*`*[T](q: Quat[T]; v: array[3,T]): array[3,T] =
  let qxv = cross(q.vec.xyz, v)
  let qxqxv = cross(q.vec.xyz, qxv)
  result = (qxv,qxqxv) <$> (a*q.vec.w + b) * T(2)
  return (v,result) <$> a+b
template `*`*[T](v: array[3,T]; q: Quat[T]): array[3,T] = q.inverse * v

func `*`*[T](q: Quat[T]; v: array[4,T]): array[4,T] = array[4,T](q * v.xyz, v.w)
template `*`*[T](v: array[4,T]; q: Quat[T]): array[4,T] = inverse(q) * v

func `-`*[T](q: Quat[T]): Quat[T] = quat -q.vec

func `+`*[T](a,b: Quat[T]): Quat[T] = quat a.vec + b.vec
func `/`*[T](a: Quat[T]; b: T): Quat[T] = quat a.vec / b

# =====================================================================// Math #
# Rotation //================================================================= #

func axis*[T](x: Quat[T]): array[3,T] =
  let tmp1: T = T(1) - x.w * x.w
  if tmp1 <= T(0):
    return [T(0),0,1]
  let tmp2: T = T(1) / sqrt(tmp1)
  [x.x*tmp2, x.y*tmp2, x.z*tmp2]

func roll*[T: SomeFloat](q: Quat[T]): Radian[T] =
  shortaccess q
  arctan2(T(2) * (qx*qy + qw*qz), qw*qw + qx*qx - qy*qy - qz*qz)

func pitch*[T: SomeFloat](q: Quat[T]): Radian[T] =
  shortaccess q
  let ay: T = T(2) * (qy*qz + qw*qx)
  let ax: T = qw*qw - qx*qx - qy*qy + qz*qz
  if ay == T(0) and ax == T(0): # avoid atan2(0,0) - handle singularity - Matiis
    T(2)*arctan2(qx,qw)
  else:
    arctan2(ay,ax)

func yaw*[T: SomeFloat](q: Quat[T]): Radian[T] =
  shortaccess q
  arcsin(clamp(T(-2) * (qx*qz - qw*qy), T(-1), T(1)))

func eulerAngles*(x: Quat[float32]): array[3,Radian32] =
  [pitch(x), yaw(x), roll(x)]
func eulerAngles*(x: Quat[float64]): array[3,Radian64] =
  [pitch(x), yaw(x), roll(x)]

func ex*[T](q: Quat[T]): array[3,T] = shortaccess q; array[3,T](
  1 - 2*(qy*qy + qz*qz),
      2*(qx*qy + qz*qw),
      2*(qx*qz - qy*qw))

func ey*[T](q: Quat[T]): array[3,T] = shortaccess q; array[3,T](
      2*(qx*qy - qz*qw),
  1 - 2*(qx*qx + qz*qz),
      2*(qx*qw + qy*qz))

func ez*[T](q: Quat[T]): array[3,T] = shortaccess q; array[3,T](
      2*(qx*qz + qy*qw),
      2*(qy*qz - qx*qw),
  1 - 2*(qx*qx + qy*qy))

func nx*[T](q: Quat[T]): Normalized[array[3,T]] = q.ex.normalize
func ny*[T](q: Quat[T]): Normalized[array[3,T]] = q.ey.normalize
func nz*[T](q: Quat[T]): Normalized[array[3,T]] = q.ez.normalize

# =================================================================// Rotation #