import ./normals
import ../math
import ../vectors
import ../angles

import ./quaternion_basic

type NQuat*[T: SomeFloat] = Normalized[Quat[T]]

func idle*[T](Q: typedesc[NQuat[T]]): Q = asnormalized quat [0.T,0,0,1]

func nquat*[T](dirFrom, dirTo: Vec[3,T]): NQuat[T] =
  quat(dirFrom, dirTo).normalize

func nquat*[T: SomeFloat](m: array[3, array[3,T]]): NQuat[T] =
  ## mat needs to be rotation matrix (orthogonal, det(mat) = 1)

  type Elem {.pure.} = enum
    x, y, z, w
  let
    `val4x^2-1` = m[0][0] - m[1][1] - m[2][2]
    `val4y^2-1` = m[1][1] - m[0][0] - m[2][2]
    `val4z^2-1` = m[2][2] - m[0][0] - m[1][1]
    `val4w^2-1` = m[0][0] + m[1][1] + m[2][2]
    (`biggest_4X^2-1`, biggestElem) = max [
      (`val4x^2-1`, Elem.x),
      (`val4y^2-1`, Elem.y),
      (`val4z^2-1`, Elem.z),
      (`val4w^2-1`, Elem.w),
    ]

  let biggest_X = sqrt(`biggest_4X^2-1` + T(1)) * T(0.5)
  let v4X = T(4) * biggest_X

  return case biggestElem
  of Elem.x:
    template x: untyped = biggest_X
    template v4x: untyped = v4X
    let
      v4yx = m[0][1] + m[1][0]
      v4zx = m[2][0] + m[0][2]
      v4wx = m[1][2] - m[2][1]
    asNormalized quat [x, v4yx/v4x, v4zx/v4x, v4wx/v4x]
  of Elem.y:
    template y: untyped = biggest_X
    template v4y: untyped = v4X
    let
      v4xy = m[0][1] + m[1][0]
      v4zy = m[1][2] + m[2][1]
      v4wy = m[2][0] - m[0][2]
    asNormalized quat [v4xy/v4y, y, v4zy/v4y, v4wy/v4y]
  of Elem.z:
    template z: untyped = biggest_X
    template v4z: untyped = v4X
    let
      v4xz = m[2][0] + m[0][2]
      v4yz = m[1][2] + m[2][1]
      v4wz = m[0][1] - m[1][0]
    asNormalized quat [v4xz/v4z, v4yz/v4z, z, v4wz/v4z]
  of Elem.w:
    template w: untyped = biggest_X
    template v4w: untyped = v4X
    let
      v4xw = m[1][2] - m[2][1]
      v4yw = m[2][0] - m[0][2]
      v4zw = m[0][1] - m[1][0]
    asNormalized quat [v4xw/v4w, v4yw/v4w, v4zw/v4w, w]

func nquat*[T](mat: array[4, array[4,T]]): NQuat[T] =
  ## mat needs to be rotation matrix (orthogonal, det(mat) = 1
  nquat [mat[0].xyz, mat[1].xyz, mat[2].xyz]


func conjugate*[T](q: NQuat[T]): NQuat[T] = asNormalized q.unwrap.conjugate
func inverse  *[T](q: NQuat[T]): NQuat[T] = q.conjugate

func `-`*[T](q : NQuat[T]): NQuat[T] = asNormalized -(q.unwrap)

template `*`*[T](a,b: NQuat[T]): NQuat[T] = asNormalized a.unwrap * b.unwrap
template `*`*[T](a: NQuat[T]; b: Normalized[array[3,T]]): Normalized[array[3,T]] = asNormalized a.unwrap * b.unwrap
template `*`*[T](a: Normalized[array[3,T]]; b: NQuat[T]): Normalized[array[3,T]] = asNormalized a.unwrap * b.unwrap

func angle*[T: SomeFloat](x: NQuat[T]): Radian[T] = arccos(x.unwrap.vec.w) * T(2)

template shortaccess(q): untyped =
  template `q x`: untyped = q.unwrap.vec.x
  template `q y`: untyped = q.unwrap.vec.y
  template `q z`: untyped = q.unwrap.vec.z
  template `q w`: untyped = q.unwrap.vec.w

func nx*[T](q: NQuat[T]): Normalized[array[3,T]] = shortaccess q; asNormalized [
  1.T - 2.T*(qy*qy + qz*qz),
        2.T*(qx*qy + qz*qw),
        2.T*(qx*qz - qy*qw)]

func ny*[T](q: NQuat[T]): Normalized[array[3,T]] = shortaccess q; asNormalized [
        2.T*(qx*qy - qz*qw),
  1.T - 2.T*(qx*qx + qz*qz),
        2.T*(qx*qw + qy*qz)]

func nz*[T](q: NQuat[T]): Normalized[array[3,T]] = shortaccess q; asNormalized [
        2.T*(qx*qz + qy*qw),
        2.T*(qy*qz - qx*qw),
  1.T - 2.T*(qx*qx + qy*qy)]
