import ./quaternion_basic
import ./quaternion_normalized
import ./vector_basic
import ./vector_normalized
import ./normals

import ../angles

template shortaccess(q): untyped =
  template `q x`: untyped = q.vec.x
  template `q y`: untyped = q.vec.y
  template `q z`: untyped = q.vec.z
  template `q w`: untyped = q.vec.w

# Type.rotate
func rotate*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]; axis: Normalized[array[3,T]]): NQ =
  asNormalized quat vec(axis*sin(angle/2), cos(angle/2))

func rotateX*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]): NQ =
  ## rotates q around X axis by the given angle in radians
  asNormalized quat [sin(angle/2), 0, 0, cos(angle/2)]

func rotateY*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]): NQ =
  ## rotates q around Y axis by the given angle in radians
  asNormalized quat [0.T, sin(angle/2), 0, cos(angle/2)]

func rotateZ*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]): NQ =
  ## rotates q around Z axis by the given angle in radians
  asNormalized quat [0.T, 0, sin(angle/2), cos(angle/2)]

func rotateXY*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQ =
  ## rotates q around XY axis by the given angle in radians
  let v = axis * sin(angle/2)
  asNormalized quat [v[0], v[1], 0, cos(angle/2)]

func rotateYZ*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQ =
  ## rotates q around YZ axis by the given angle in radians
  let v = axis * sin(angle/2)
  asNormalized quat [0.T, v[0], v[1], cos(angle/2)]

func rotateXZ*[T](NQ: typedesc[NQuat[T]]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQ =
  ## rotates q around XZ axis by the given angle in radians
  let v = axis * sin(angle/2)
  asNormalized quat [v[0], 0, v[1], cos(angle/2)]

# inst.rotate
func rotate*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[3,T]]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  q * NQuat[T].rotate(angle, axis)

func rotateX*[T](q: Quat[T]; angle: Radian[T]): Quat[T] =
  ## rotates q around X axis by the given angle in radians
  shortaccess q
  let vx = sin(angle/2)
  let vw = cos(angle/2)
  quat [
    qw*vx + qx*vw,
    qy*vw + qz*vx,
    qz*vw - qy*vx,
    qw*vw - qx*vx ]

func rotateY*[T](q: Quat[T]; angle: Radian[T]): Quat[T] =
  ## rotates q around Y axis by the given angle in radians
  shortaccess q
  let vy = sin(angle/2)
  let vw = cos(angle/2)
  quat [
    qx*vw - qz*vy,
    qw*vy + qy*vw,
    qz*vw + qx*vy,
    qw*vw - qy*vy ]

func rotateZ*[T](q: Quat[T]; angle: Radian[T]): Quat[T] =
  ## rotates q around Z axis by the given angle in radians
  shortaccess q
  let vz = sin(angle/2)
  let vw = cos(angle/2)
  quat [
    qx*vw + qy*vz,
    qy*vw - qx*vz,
    qw*vz + qz*vw,
    qw*vw - qz*vz ]

func rotateXY*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): Quat[T] =
  ## rotates q around XY axis by the given angle in radians
  shortaccess q
  let v = axis * sin(angle/2)
  let vw = cos(angle/2)
  template vx: untyped = v.x
  template vy: untyped = v.y
  quat [
    qw*vx + qx*vw - qz*vy,
    qw*vy + qy*vw + qz*vx,
    qz*vw + qx*vy - qy*vx,
    qw*vw - qx*vx - qy*vy ]

func rotateYZ*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): Quat[T] =
  ## rotates q around YZ axis by the given angle in radians
  shortaccess q
  let v = axis * sin(angle/2)
  let vw = cos(angle/2)
  template vy: untyped = v.x
  template vz: untyped = v.y
  quat [
    qx*vw + qy*vz - qz*vy,
    qw*vy + qy*vw - qx*vz,
    qw*vz + qz*vw + qx*vy,
    qw*vw - qy*vy - qz*vz ]

func rotateXZ*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): Quat[T] =
  ## rotates q around XZ axis by the given angle in radians
  shortaccess q
  let v = axis * sin(angle/2)
  let vw = cos(angle/2)
  template vx: untyped = v.x
  template vz: untyped = v.y
  quat [
    qw*vx + qx*vw + qy*vz,
    qy*vw + qz*vx - qx*vz,
    qw*vz + qz*vw - qy*vx,
    qw*vw - qx*vx - qz*vz ]


func rotateGrobal*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[3,T]]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  quat(axis, angle) * q

func rotateGrobalX*[T](q: Quat[T]; angle: Radian[T]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  shortaccess q
  let x = sin(angle/2)
  let w = cos(angle/2)
  quat [
    w*qx + x*qw,
    w*qy - x*qz,
    w*qz + x*qy,
    w*qw - x*qx ]

func rotateGrobalY*[T](q: Quat[T]; angle: Radian[T]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  shortaccess q
  let y = sin(angle/2)
  let w = cos(angle/2)
  quat [
    w*qx + y*qz,
    w*qy + y*qw,
    w*qz - y*qx,
    w*qw - y*qy ]

func rotateGrobalZ*[T](q: Quat[T]; angle: Radian[T]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  shortaccess q
  let z = sin(angle/2)
  let w = cos(angle/2)
  quat [
    w*qx - z*qy,
    w*qy + z*qx,
    w*qz + z*qw,
    w*qw - z*qz ]

func rotateGrobalXY*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  shortaccess q
  let v = axis * sin(angle/2)
  let w = cos(angle/2)
  template x: untyped = v.x
  template y: untyped = v.y
  quat [
    w*qx + x*qw + y*qz,
    w*qy + y*qw - x*qz,
    w*qz + x*qy - y*qx,
    w*qw - x*qx - y*qy ]

func rotateGrobalYZ*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  shortaccess q
  let v = axis * sin(angle/2)
  let w = cos(angle/2)
  template y: untyped = v.x
  template z: untyped = v.y
  quat [
    w*qx + y*qz - z*qy,
    w*qy + y*qw + z*qx,
    w*qz + z*qw - y*qx,
    w*qw - y*qy - z*qz ]

func rotateGrobalXZ*[T](q: Quat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): Quat[T] =
  ## rotates q around axis by the given angle in radians
  shortaccess q
  let v = axis * sin(angle/2)
  let w = cos(angle/2)
  template x: untyped = v.x
  template z: untyped = v.y
  quat [
    w*qx + x*qw - z*qy,
    w*qy + z*qx - x*qz,
    w*qz + z*qw + x*qy,
    w*qw - x*qx - z*qz ]

func rotate*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[3,T]]): NQuat[T] =
  ## rotates q around axis by the given angle in radians
  q.unwrap.rotate(angle, axis).asNormalized

func rotateX*[T](q: NQuat[T]; angle: Radian[T]): NQuat[T] =
  q.unwrap.rotateX(angle).asNormalized
func rotateY*[T](q: NQuat[T]; angle: Radian[T]): NQuat[T] =
  q.unwrap.rotateY(angle).asNormalized
func rotateZ*[T](q: NQuat[T]; angle: Radian[T]): NQuat[T] =
  q.unwrap.rotateZ(angle).asNormalized
func rotateXY*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQuat[T] =
  q.unwrap.rotateXY(angle, axis).asNormalized
func rotateYZ*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQuat[T] =
  q.unwrap.rotateYZ(angle, axis).asNormalized
func rotateXZ*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQuat[T] =
  q.unwrap.rotateXZ(angle, axis).asNormalized

func rotateGrobal*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[3,T]]): NQuat[T] =
  q.unwrap.rotateGrobal(angle, axis).asNormalized

func rotateGrobalX*[T](q: NQuat[T]; angle: Radian[T]): NQuat[T] =
  q.unwrap.rotateGrobalX(angle).asNormalized
func rotateGrobalY*[T](q: NQuat[T]; angle: Radian[T]): NQuat[T] =
  q.unwrap.rotateGrobalY(angle).asNormalized
func rotateGrobalZ*[T](q: NQuat[T]; angle: Radian[T]): NQuat[T] =
  q.unwrap.rotateGrobalZ(angle).asNormalized
func rotateGrobalXY*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQuat[T] =
  q.unwrap.rotateGrobalXY(angle, axis).asNormalized
func rotateGrobalYZ*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQuat[T] =
  q.unwrap.rotateGrobalYZ(angle, axis).asNormalized
func rotateGrobalXZ*[T](q: NQuat[T]; angle: Radian[T]; axis: Normalized[array[2,T]]): NQuat[T] =
  q.unwrap.rotateGrobalXZ(angle, axis).asNormalized

