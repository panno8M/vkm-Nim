#[ Package ]#

version       = "1.0.0"
author        = "panno"
description   = "GLM impremented with Nim"
license       = "MIT"

requires "nim >= 1.6.0"

task bench, "":
  exec "nim c -r -d:release --gc:orc --hints:off tests/bench"
  exec "rm tests/bench"