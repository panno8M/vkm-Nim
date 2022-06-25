##Vector module contains all types and functions to manipulate vectors

import ./details/vector_basic;      export vector_basic
import ./details/vector_normalized; export vector_normalized
import ./details/vector_swizzle;    export vector_swizzle


when isMainModule:
  import ./normals
  import ./functionalops
  import std/sugar
  # import vkm/math/interpolation
  import vkm/math
  import vkm/angles
  var V = [1d, 2, 3]

  V.xy = [4d, 5]
  V.xz = [6d, 7]

  var NV = V.normalize
  echo repr NV
  echo NV.unwrap.norm2

  var NV2 = normalize [4d, 5, 12]
  echo repr NV2
  echo NV2.unwrap.norm2

  echo dot(\NV, \NV2)

  echo V.xxyz

  var Vrad = [PI.rad, 30'deg.rad, 45'deg.rad]
  echo Vrad <$> sin(a) <$> arcsin(a)
  echo Vrad <$> sin(a) <$> $a


  var v0 = [1d, 0.5, 0]
  var u0 = [10d, 10, 0]
  discard cross(v0,u0)

  var v1 = [1,2,3,4] div 2

  v1.yz += [10,10]

  v1.zw = v1.zw div [3,3]

  doAssert v1 == [0, 11, 3, 0]

  # for v in slerp([1d, 0], [1d, 1].normalize*2, 10):
  #   echo "=================="
  #   dump v
  #   dump v.norm
