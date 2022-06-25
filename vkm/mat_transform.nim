import ./globals
import ./angles
import ./quaternions
import ./matrices
import ./vectors

import ./details/normals

{.push, inline.}

func poseMatrix*[T](translate: array[3,T]; rotate: NQuat[T]; scale: array[3,T]): Mat[4,4,T] =
  let
    factor1 = rotate.mat3
    factor2 = scale.diag

  let scalerot_mat = factor1 * factor2

  result[0] = vec(scalerot_mat[0], 0.T)
  result[1] = vec(scalerot_mat[1], 0.T)
  result[2] = vec(scalerot_mat[2], 0.T)
  result[3] = vec(translate,    1.T)

func pickMatrix*[T](center, delta: array[2,T]; viewport: array[4,T]): Mat[4,4,T] =
  ## Define a picking region
  assert(delta.x > T(0) and delta.y > T(0))
  result = mat4(T(1))

  let deltaPos = [
    (viewport[2] - T(2) * (center.x - viewport[0])) / delta.x,
    (viewport[3] - T(2) * (center.y - viewport[1])) / delta.y,
    ]

  # Translate and scale the picked region to the entire window
  result.w.xy = deltaPos
  result.xyz *= [viewport[2]/delta.x, viewport[3]/delta.y, 1]

func ortho*[T]( left, right, bottom, top, zNear, zFar:T): Mat[4,4,T] =
    result = mat4[T](1.0)
    result[0][0] = T(2) / (right - left)
    result[1][1] = T(2) / (top - bottom)
    result[2][2] = -T(2) / (zFar - zNear)
    result[3][0] = -(right + left) / (right - left)
    result[3][1] = -(top + bottom) / (top - bottom)
    result[3][2] = -(zFar + zNear) / (zFar - zNear)
    result[3][3] = 1

func perspectiveLH*[T](fovy: Radian[T]; aspect, zNear, zFar:T): Mat[4,4,T] =
    let tanHalfFovy = tan(fovy / 2)
    result[0][0] = 1 / (aspect * tanHalfFovy)
    result[1][1] = 1 / (tanHalfFovy)
    result[2][2] = (zFar + zNear) / (zFar - zNear)
    result[2][3] = 1
    result[3][2] = - (2 * zFar * zNear) / (zFar - zNear)

func perspectiveRH*[T](fovy: Radian[T]; aspect, zNear, zFar:T): Mat[4,4,T] =
    let tanHalfFovy = tan(fovy / 2)
    result[0][0] = 1 / (aspect * tanHalfFovy)
    result[1][1] = 1 / (tanHalfFovy)
    result[2][2] = -(zFar + zNear) / (zFar - zNear)
    result[2][3] = -1
    result[3][2] = -(2 * zFar * zNear) / (zFar - zNear)

func perspective*[T](fovy: Radian[T]; aspect,zNear,zFar: T): Mat[4,4,T]=
  when System == LeftHand:
    perspectiveLH(fovy, aspect, zNear, zFar)
  else:
    perspectiveRH(fovy, aspect, zNear, zFar)

func viewMat[T](pos: array[3,T]; front, right, top: Normalized[array[3,T]]): Mat[4,4,T] {.inline.} =
    result{0} = vec(right.unwrap, 0.T)
    result{1} = vec(top.unwrap, 0.T)
    result{2} = vec(front.unwrap, 0.T)
    result[3] = vec(
      -dot(right.unwrap, pos),
      -dot(top.unwrap, pos),
      -dot(front.unwrap, pos),
      1.T)

func lookForRH*[T](eye: array[3,T]; ray, worldup: Normalized[array[3,T]]): Mat[4,4,T] {.inline.} =
  let front = -ray
  let side = ray.cross worldup
  let up = -ray.cross side
  viewMat(eye, front, side, up)

func lookForLH*[T](eye: array[3,T]; ray, worldup: Normalized[array[3,T]]): Mat[4,4,T] {.inline.} =
  let front = ray
  let side = -ray.cross worldup
  let up = ray.cross side
  viewMat(eye, front, side, up)

func lookFor*[T](eye: array[3,T]; ray, up: Normalized[array[3,T]]): Mat[4,4,T]=
  when System == LeftHand:
    lookForLH(eye, ray, up)
  else:
    lookForRH(eye, ray, up)

func lookAtRH*[T](eye,center: array[3,T]; up: Normalized[array[3,T]]): Mat[4,4,T] =
  lookForRH(eye, (center-eye).normalize, up)
func lookAtLH*[T](eye,center: array[3,T]; up: Normalized[array[3,T]]):Mat[4,4,T]=
  lookForLH(eye, (center-eye).normalize, up)

func lookAt*[T](eye,center: array[3,T]; up: Normalized[array[3,T]]): Mat[4,4,T]=
  when System == LeftHand:
    lookAtLH(eye, center, up)
  else:
    lookAtRH(eye, center, up)

func frustum*[T](left, right, bottom, top, near, far: T): Mat[4,4,T] =
  result[0][0] =       (2*near)/(right-left)
  result[1][1] =       (2*near)/(top-bottom)
  result[2][2] =     (far+near)/(near-far)
  result[2][0] =   (right+left)/(right-left)
  result[2][1] =   (top+bottom)/(top-bottom)
  result[2][3] = -1
  result[3][2] =   (2*far*near)/(near-far)


when isMainModule:
  let lookAtMat = [
    [0.0, -0.707106, -0.707106, 0.0],
    [0.0,  0.707106, -0.707106, 0.0],
    [1.0,  0.0     ,  0.0     , 0.0],
    [0.0,  0.0     ,  0.0     , 1.0]]

  let frustumMat = [
    [4.0, 0.0 ,   0.0,  0.0],
    [0.0, 1.0 ,   0.0,  0.0],
    [0.1, 0.25,  -2.0, -1.0],
    [0.0, 0.0 , -15.0,  0.0]]

  discard ortho(-5.0, 5, -5,5, 0.01, 100)
  var la = lookAt([0d,0,0], [50d, 50, 0], asNormalized [0d,1,0])
  echo $la
  doAssert all nearlyEqual(la, lookAtMat, 1e-5)

  let frst = frustum(-1.125, 1.375, -03.75, 06.25, 5, 15)
  echo "frustum matrix:"
  echo frst
  doAssert all nearlyEqual(frst, frustumMat, 1e-5)
