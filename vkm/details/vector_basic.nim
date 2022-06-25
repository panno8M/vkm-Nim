import std/macros
import std/sequtils

import ../angles
import ../etctypes
import ../math
import ../functionalops

# Type //===================================================================== #

type
  SomeVecElem* = SomeNumber|bool|SomeAngle|SomeNorm|SomeScaled

  Vec*[I: static int; T: SomeVecElem] = array[I, T]

# Constructor //============================================================== #

macro vec*(exp: varargs[typed]): untyped =
  result = newStmtList()
  var vecgenstmt = nnkBracket.newNimNode()
  for i, e in exp:
    if e.typeKind == ntyArray:
      var alias = e
      if e.kind in nnkCallKinds:
        alias = ident("arg" & $i)
        result.add quote do:
          let `alias` {.genSym.} = `e`

      let high = e.getTypeImpl[1][2].intVal
      for i in 0..high:
        vecgenstmt.add nnkBracketExpr.newTree(alias, newlit i)
    else:
      vecgenstmt.add e

  result.add vecgenstmt


# ================================================================// Construct #
# Math //===================================================================== #

func `all`*[N](x: array[N, bool]): bool = x.foldl(a and b)
func `any`*[N](x: array[N, bool]): bool = x.foldl(a or b)

template genOp2args(op): untyped =
  func op*[N; T](v,u: array[N,T])    : array[N,T] = (v,u).fmap op(a,b)
  func op*[N; T](v: array[N,T]; s: T): array[N,T] = (v,s).fmap op(a,b)
  func op*[N; T](s: T; v: array[N,T]): array[N,T] = (s,v).fmap op(a,b)
macro genOps2args(ops: varargs[untyped]): untyped =
  newStmtList ops.mapIt ident"genOp2args".newCall(it)

template genOp1arg(op): untyped =
  when defined release:
    template op*[N; T](v: array[N,T]): array[N,T] = v.fmap op(a)
  else:
    func op*[N; T](v: array[N,T]): array[N,T] = v.fmap op(a)
macro genOps1arg(ops: varargs[untyped]): untyped =
  newStmtList ops.mapIt ident"genOp1arg".newCall(it)

genOps1arg(`floor`, `fract`, `abs`)
genOps2args(`+`, `-`, `*`, `/`, `div`, `mod`, `floorMod`, `min`, `max`, `pow`)

template `+=`*(a,b: untyped): untyped = a = a + b
template `-=`*(a,b: untyped): untyped = a = a - b
template `*=`*(a,b: untyped): untyped = a = a * b
template `/=`*(a,b: untyped): untyped = a = a / b

func  `<` *[N; T](v,u: array[N,T]): array[N,bool] = (v,u).fmap(a < b)
func `<=` *[N; T](v,u: array[N,T]): array[N,bool] = (v,u).fmap(a <= b)

# unary operators
func `-`*[N; T](v: array[N,T]): array[N,T] = v.fmap(-a)
template `+`*[N; T](v: array[N,T]): array[N,T] = v

func nearlyEqual*[N: static int; T](v1,v2: array[N, T]; allowableError: static T = 5e-7): array[N,bool] =
  (v1,v2).fmap(nearlyEqual(a,b,allowableError))

func clamp*[N; T](arg,minVal,maxVal: array[N,T]): array[N,T] =
  for i in 0..<N: result[i] = clamp(arg[i], minVal[i], maxVal[i])

func clamp*[N; T](arg: array[N,T]; minVal,maxVal: T): array[N,T] =
  arg.fmap(a.clamp(minVal,maxVal))

# Geometric Functions
func dot*[N; T](v,u:  array[N,T]): T = sum (v,u) <$> a * b

func norm2*[N; T](v: array[N,T]): T = dot(v, v)
func norm*[N; T](v: array[N,T]): T = sqrt v.norm2


func cross*[T](v1,v2:array[3,T]): array[3,T] =
  [ v1.y*v2.z - v1.z*v2.y,
    v1.z*v2.x - v1.x*v2.z,
    v1.x*v2.y - v1.y*v2.x]

func distance*[N; T](v1,v2: array[N,T]): T = norm(v2 - v1)

func faceforward*[N; T](n,i,nref: array[N,T]): array[N,T] =
  ## return a vector pointing in the same direction as another
  n * (float(dot(nref, i) < 0) * 2 - 1)

func refract*[N; T](i,n: array[N,T]; eta: T): array[N,T] =
  # For a given incident vector ``i``, surface normal ``n`` and ratio of indices of refraction, ``eta``, refract returns the refraction vector.
  let k = 1 - eta * eta * (1 - dot(n, i) * dot(n, i))
  if k >= 0.0:
    result = eta * i - (eta * dot(n, i) + sqrt(k)) * n


# =====================================================================// Math #
# Utility //================================================================== #

## Address getter to pass vector to native-C openGL functions as pointers
func head*[N; T](v: var array[N,T]): ptr T = v[0].addr


# ==================================================================// Utility #