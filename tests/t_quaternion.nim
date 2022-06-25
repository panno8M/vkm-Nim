import std/unittest

import vkm

let axis = (
  x: [1f, 0, 0],
  y: [0f, 1, 0],
  z: [0f, 0, 1],
  nx: /![1f, 0, 0],
  ny: /![0f, 1, 0],
  nz: /![0f, 0, 1])

suite "quaternion":
  test "direction":
    let Y90rot = NQuat[float32].rotateY 90'deg32.rad
    check all (Y90rot * axis.nx) ~= -axis.nz
  test "angle":
    block:
      let N = NQuat[float32].rotateZ 45'deg32.rad
      check N.unwrap.vec.norm ~= 1
      check nearlyEqual(N.angle, 45'deg32, 1e-3)

    block:
      let N = NQuat[float32].rotate(45'deg32.rad, //[0f,1,1])
      check N.unwrap.vec.norm ~= 1
      check nearlyEqual(N.angle, 45'deg32, 1e-3)

    block:
      let N = NQuat[float32].rotate(45'deg32.rad, //[1f,2,3])
      check N.unwrap.vec.norm ~= 1
      check N.angle ~= 45'deg32

  test "mix":
    let A = NQuat[float32].rotateZ 0'deg32.rad
    let B = NQuat[float32].rotateZ 90'deg32.rad
    let C = slerp(A, B, 0.5'fac)
    let D = NQuat[float32].rotateZ 45'deg32.rad

    check all C ~= D

  test "normalize":
    block:
      let N = NQuat[float32].rotateZ 45'deg32.rad
      check N.unwrap.vec.norm ~= 1

    block:
      let N = NQuat[float32].rotate(45'deg32.rad, //[0f,0,2])
      check N.unwrap.vec.norm ~= 1

    block:
      let N = NQuat[float32].rotate(45'deg32.rad, //[1f,2,3])
      check N.unwrap.vec.norm ~= 1

  test "euler":
    block:
      let q = quat [0f,0,1,1]
      let Angles: array[3,Radian32]  = q.eulerAngles
      check Angles.x == q.yaw
      check Angles.y == q.pitch
      check Angles.z == q.roll

  test "mul":
    let temp1 = NQuat[float32].rotateY 1'rad32
    let temp2 = NQuat[float32].rotateX 0.5'rad32

    let transformed0: array[3,float32] = temp1 * axis.y * temp1.inverse
    discard temp2 * transformed0 * temp2.inverse

    let temp3: NQuat[float32] = temp1 * temp2
    discard temp3 * axis.y * temp3.inverse

    var temp4 = NQuat[float32].idle
    temp4 *= temp3
    temp4 *= temp3.inverse

    check all nearlyEqual(temp4, NQuat[float32].idle, 1e-2)

  test "two axis ctr":
    template `~=`(a, b: untyped): untyped = nearlyEqual(a, b, 1e-4)
    let q1: NQuat[float32] = nquat[float32](axis.x, axis.y)
    let v1: array[3,float32] = q1 * axis.x
    check all v1 ~= axis.y

    let q2: NQuat[float32] = q1 * q1
    let v2: array[3,float32] = q2 * axis.x
    check all v2 ~= -axis.x

  test "type":
    var A: Quat[float32]
    var B: Quat[float64]
    check A.typeof is Quat[float32]
    check B.typeof is Quat[float64]


  test "mul vec":
    let q = NQuat[float32].rotateZ 90'deg32.rad
    let u: array[3,float32] = q * axis.x
    let w: array[3,float32] = u * q

    check all w ~= axis.x

  test "size":
    check 16 == sizeof(Quat[float32])
    check 32 == sizeof(Quat[float64])

  test "mat convert":
    let a = 1.0f / sqrt(2.0f)
    let b = 1.0f / sqrt(3.0f)

    for q in [/!quat [1f,0,0,0], /!quat [0f,1,0,0], /!quat [0f,0,1,0], /!quat [0f,0,0,1],
              /!quat [a ,a,0,0], /!quat [a ,0,a,0], /!quat [a ,0,0,a], /!quat [0f,a,a,0], /!quat [0f,a,0,a], /!quat [0f,0,a,a],
              /!quat [0f,b,b,b], /!quat [b ,0,b,b], /!quat [b ,b,0,b], /!quat [b ,b,b,0]]:
      let q2 = nquat mat3(q)
      check all q ~= q2

  test "rotate":
    check all NQuat[float32].idle * axis.nx ~= axis.nx

    let HalfPi = PI.rad32/2

    let axis3 = [axis.nx, axis.ny, axis.nz]
    let data = [
      (HalfPi,  axis.nx, [ axis.nx, axis.nz,-axis.ny]),
      (HalfPi,  axis.ny, [-axis.nz, axis.ny, axis.nx]),
      (HalfPi,  axis.nz, [ axis.ny,-axis.nx, axis.nz]),
      (HalfPi, -axis.nx, [ axis.nx,-axis.nz, axis.ny]),
      (HalfPi, -axis.ny, [ axis.nz, axis.ny,-axis.nx]),
      (HalfPi, -axis.nz, [-axis.ny, axis.nx, axis.nz]),
    ]

    for angle, axis, res in data.items:
      let q = NQuat[float32].rotate(angle, axis)
      for i in 0..2:
        check all q * axis3[i] ~= res[i]

    let q = NQuat[float32]
      .rotateX(HalfPi)
      .rotateY(HalfPi)

    check all q * axis.nz ~= axis.nx

  test "axis":
    let q = NQuat[float32]
      .rotateX(45'deg32.rad)
      .rotateY(45'deg32.rad)

    check q.unwrap.norm ~= 1
    check q.nx.unwrap.norm ~= 1
    check q.ny.unwrap.norm ~= 1
    check q.nz.unwrap.norm ~= 1