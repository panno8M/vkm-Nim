import ./details/quaternion_basic;      export quaternion_basic
import ./details/quaternion_normalized; export quaternion_normalized
import ./details/quaternion_rotation;   export quaternion_rotation

when isMainModule:
  let q1 = quat [1f,2,3,4]
  let q2 = inverse(q1)

  echo $(q1 * q2)
  echo q2 * q1
