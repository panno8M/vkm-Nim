import std/unittest
import std/sequtils
import vkm

func plot(angle: Degree32): Normalized[array[2,float32]] =
  /! [cos(angle), sin(angle)]

suite "interpolation":
  test "vector slerp":
    let v0deg   = plot 0'deg32
    let v30deg  = plot 30'deg32
    let v60deg  = plot 60'deg32
    let v120deg = plot 120'deg32
    let v180deg = plot 180'deg32
    let v240deg = plot 240'deg32
    let v300deg = plot 300'deg32

    check all slerp(v0deg, v60deg, 0.5'fac) ~= v30deg
    check all slerp(v60deg, v0deg, 0.5'fac) ~= v30deg


    check all slerp(v0deg, v120deg, 0.5'fac) ~= v60deg
    check all slerp(v120deg, v0deg, 0.5'fac) ~= v60deg

    doAssertRaises(DivByZeroDefect):
      for v in slerp(v0deg, v180deg, 10): discard v

    check all slerp(v0deg, v240deg, 0.5'fac) ~= v300deg
    check all slerp(v240deg, v0deg, 0.5'fac) ~= v300deg

    for v in lerp(v0deg.unwrap, v180deg.unwrap, 10):
      discard v

  test "quaternion slerp":
    let id = NQuat[float32].idle
    let y45deg = NQuat[float32].rotateY(45'deg32.rad)
    let y90deg = NQuat[float32].rotateY(90'deg32.rad)

    # Must be id
    check all slerp(id, y90deg, 0'fac) ~= id
    # Must be 90° rotation on Y : 0 0.7 0 0.7
    check all slerp(id, y90deg, 1'fac) ~= y90deg

    # Must be 45° rotation on Y : 0 0.38 0 0.92
    check all slerp(y90deg, id, 0.5'fac) ~= y45deg
    check all slerp(id, y90deg, 0.5'fac) ~= y45deg

    # Testing against full circle around the sphere instead of shortest path
    # Must be 45° rotation on Y
    # certainly not a 135° rotation
    check all slerp(id, -y90deg, 0.5'fac) ~= y45deg
    check [y45deg, -y45deg].anyIt do:
      all slerp(-y90deg, id, 0.5'fac) ~= it
    check slerp(id, -y90deg, 0.5'fac).angle ~= 45'deg32

    # Must be 90° rotation on Y : 0 0.7 0 0.7
    check all slerp(y90deg, y90deg, 0.5'fac) ~= y90deg

    # Must be 90° rotation on almost any axis that is on the XZ plane
    check slerp(id, -y90deg, 0.5'fac).angle ~= 45'deg32

    # Must be id
    check all slerp(id, -id, 0.5'fac) ~= id
    check all slerp(-id, id, 0.5'fac) ~= -id