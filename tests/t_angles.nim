import std/unittest
import vkm/math
import vkm/angles
from std/math as stdmath import sin

test "angles coversion":
  check (15'deg32 * 2) ~= (PI/6).rad.deg32
  check sin(30'deg.rad) == sin(PI/6)
  check sin((PI/6).rad) == sin(PI/6)
