import std/macros
import std/sequtils

import ./vectors
import ./functionalops
import ./quaternions

# Type //===================================================================== #

type
  Mat*[Col,Row: static int; T] = array[Col, array[Row, T]]


# =====================================================================// Type #
# Constructor //============================================================== #

{.push, inline.}

func mat4*[T](v: array[4,T]): Mat[4,4,T] = [
  [v[0],    0,    0,    0],
  [T(0), v[1],    0,    0],
  [T(0),    0, v[2],    0],
  [T(0),    0,    0, v[3]]]
func mat3*[T](v: array[3,T]): Mat[3,3,T] = [
  [v[0],    0,    0],
  [T(0), v[1],    0],
  [T(0),    0, v[2]]]
func mat2*[T](v: array[2,T]): Mat[2,2,T] = [
  [v[0],    0],
  [T(0), v[1]]]

func mat4*[T](s: T): Mat[4,4,T] = [
  [   s, 0, 0, 0],
  [T(0), s, 0, 0],
  [T(0), 0, s, 0],
  [T(0), 0, 0, s]]
func mat3*[T](s: T): Mat[3,3,T] = [
  [   s, 0, 0],
  [T(0), s, 0],
  [T(0), 0, s]]
func mat2*[T](s: T): Mat[2,2,T] = [
  [   s, 0],
  [T(0), s]]

func mat4*[T](): Mat[4,4,T] = [
  [T(1), 0, 0, 0],
  [T(0), 1, 0, 0],
  [T(0), 0, 1, 0],
  [T(0), 0, 0, 1]]
func mat3*[T](): Mat[3,3,T] = [
  [T(1), 0, 0],
  [T(0), 1, 0],
  [T(0), 0, 1]]
func mat2*[T](): Mat[2,2,T] = [
  [T(1), 0],
  [T(0), 1]]

func diag*[N: static int,T](v : array[N,T]): Mat[N,N,T] =
  for i in 0 ..< N: result[i][i] = v[i]

{.pop}


# ==============================================================// Constructor #
# To Mat //=================================================================== #

func mat3*[T](q : NQuat[T]) : Mat[3,3,T] =
  template qx: untyped = q.unwrap.vec.x
  template qy: untyped = q.unwrap.vec.y
  template qz: untyped = q.unwrap.vec.z
  template qw: untyped = q.unwrap.vec.w
  let
    (txx, tyy, tzz) = (2*qx*qx, 2*qy*qy, 2*qz*qz)
    (txy, txz, tyz) = (2*qx*qy, 2*qx*qz, 2*qy*qz)
    (txw, tyw, tzw) = (2*qx*qw, 2*qy*qw, 2*qz*qw)

  [ [1 - tyy - tzz,     txy + tzw,     txz - tyw],
    [    txy - tzw, 1 - txx - tzz,     tyz + txw],
    [    txz + tyw,     tyz - txw, 1 - txx - tyy]]

func mat4*[T](q: NQuat[T]; v: array[4,T]): Mat[4,4,T] =
  let tmp = q.mat3
  [vec(tmp[0],0.T), vec(tmp[1],0.T), vec(tmp[2],0.T), v]

func mat4*[T](q: NQuat[T]; v: array[3,T]): Mat[4,4,T] = mat4(q, vec(v,1.T))
func mat4*[T](q: NQuat[T]): Mat[4,4,T] = mat4(q, [0.T,0,0,1])

# ===================================================================// To Mat #
# Accessor //================================================================= #

{.push, inline.}
# setter
func `{}=`*[Col,Row; T](m: var Mat[Col,Row, T]; row: int; v: array[Col,T]) =
  for col in 0..<Col:
    m[col][row] = v[col]

func `diag=`*[Col,Row, U: static int; T](m : var Mat[Col,Row, T]; v: array[U, T]) =
  static: assert U == min(Col,Row)
  for i in 0 ..< U: m[i][i] = v[i]

# immutable getter
func `{}`*[Col,Row; T](m: Mat[Col,Row, T]; row: int): array[Col,T] =
  for col in 0..<Col: result[col] = m[col][row]

func diag*[Col,Row; T](m : Mat[Col,Row, T]): array[min(Col,Row), T] =
  for i in 0 ..< min(Col,Row): result[i] = m[i][i]

{.pop}

# =================================================================// Accessor #
# Stringify //================================================================ #

func spaces(num: int): string =
  result = newString(num)
  for c in result.mitems:
    c = ' '

func alignRight*[N,T](v: array[N, T]) : array[N,string] =
  var maxLen = 0
  for i, val in v:
    result[i] = $val
    maxLen = max(maxLen, result[i].len)
  for i, str in result.mpairs:
    str.insert(spaces(maxLen - str.len))

func alignLeft*[N,T](v: array[N, T]) : array[N,string] =
  var maxLen = 0
  for i, val in v:
    result[i] = $val
    maxLen = max(maxLen, result[i].len)
  for i, str in result.mpairs:
    str.add(spaces(maxLen - str.len))

func alignChar*[N,T](v: array[N, T]; c: char) : array[N,string] =
  for i, val in v:
    result[i] = $val

  var lenLeft  : array[N, int]
  var maxLenLeft = 0
  var lenRight : array[N, int]
  var maxLenRight = 0

  for i, str in result:
    let index = str.find(c)
    let length = str.len
    lenLeft[i]  = index
    maxLenLeft = max(maxLenLeft, lenLeft[i])
    lenRight[i] = length - index - 1
    maxLenRight = max(maxLenRight, lenRight[i])

  for i, str in result.mpairs:
    str.insert(spaces(maxLenLeft  - lenLeft[i]))
    str.add(   spaces(maxLenRight - lenRight[i]))

func columnFormat*[N,T](v: array[N, T]) : array[N,string] =
  when T is SomeInteger:
    result = v.alignRight
  elif T is SomeFloat:
    result = v.alignChar('.')
  else:
    result = v.alignLeft

const deco =
  # On a windows terminal, we are still in the 80s
  when defined(noUnicode) or defined(windows):
    [" / ", " \\ ", "|  ", " \\ ", " / ", "  |"]
  else:
    ["⎡", "⎣", "⎢", "⎤", "⎦", "⎥"]

func `$`*[Col,Row: static int; T](m: Mat[Col,Row, T]): string =
  var strmat: Mat[Col,Row, string]
  for col in 0..<Col:
    strmat[col] = columnFormat(m[col])

  for row in 0..<Row:
    result.add case row
    of 0:     deco[0]
    of Row-1: deco[1]
    else:     deco[2]

    for col in 0..<Col:
      if col != 0: result &= "  "
      result &= strmat[col][row]

    result.add case row
    of 0:     deco[3]
    of Row-1: deco[4]
    else:     deco[5]

    result &= "\n"


# ================================================================// Stringify #
# Utility //================================================================== #

{.push, inline.}
func caddr*[Col,Row; T](m: var Mat[Col,Row, T]): ptr T = m.asArr[0].addr
{.pop.}

template numCols*[Col,Row; T](t : typedesc[Mat[Col,Row, T]]): int = Col
template numRows*[Col,Row; T](t : typedesc[Mat[Col,Row, T]]): int = Row


# ==================================================================// Utility #
# Math //===================================================================== #

{.push, inline, noinit.}
proc `all`*[Col,Row: static int](m: Mat[Col,Row, bool]): bool = all m.fmap(all a)
proc `any`*[Col,Row: static int](m: Mat[Col,Row, bool]): bool = any m.fmap(any a)

func nearlyEqual*[Col,Row: static int; T](m1, m2: Mat[Col,Row, T]; allowableError: static[T] = 5e-7): Mat[Col,Row, bool] =
  (m1,m2).fmap(nearlyEqual(a,b,allowableError))

func `*`*[N,Nprime: static int; T](m: Mat[Nprime,N, T]; v: array[N, T]): array[Nprime, T] =
  for np in 0..<Nprime:
    result += m[np] * v[np]

func `*`*[N,Nprime: static int; T](v: array[N, T]; m: Mat[N,Nprime, T]): array[Nprime, T] =
  for np in 0..<Nprime:
    result[np] = dot(v, m[np])

func `*`*[Col,Row,N; T](m1: Mat[N,Row, T]; m2: Mat[Col,N, T]): Mat[Col,Row,T] =
  for col in 0..<Col:
    for row in 0..<Row:
      for n in 0..<N:
        result[col][row] += m1[n][row] * m2[col][n]

func `*`*[Col,Row; T](m: Mat[Col,Row, T]; s: T): Mat[Col,Row, T] = (m, s).fmap(a * b)
template `*`*[Col,Row; T](s: T; m: Mat[Col,Row, T]): Mat[Col,Row, T] = m * s

proc `*=`*[Col,Row; T](m: var Mat[Col,Row, T]; s: T) =
  for col in 0..<Col: m[col] *= s

proc `*=`*[N; T](m1: var Mat[N,N, T]; m2: Mat[N,N, T]) = m1 = m1 * m2

func `-`*[Col, Row; T](m: Mat[Col,Row, T]): Mat[Col,Row, T] =
  for col in 0..<Col:
    for row in 0..<Row:
      result[col][row] = -m[col][row]

func det*[T](m: Mat[2,2,T]): T =
   m[0][0] * m[1][1] - m[1][0] * m[0][1]

func det*[T](m: Mat[3,3,T]): T =
  m[0][0] * (m[1][1] * m[2][2] - m[2][1] * m[1][2]) -
  m[1][0] * (m[0][1] * m[2][2] - m[2][1] * m[0][2]) +
  m[2][0] * (m[0][1] * m[1][2] - m[1][1] * m[0][2])

func det*[T](m: Mat[4,4,T]): T =
  if m[0][0] != 0:
    result += m[0][0] * det([m[1].yzw, m[2].yzw, m[3].yzw])
  if m[0][1] != 0:
    result -= m[0][1] * det([m[1].xzw, m[2].xzw, m[3].xzw])
  if m[0][2] != 0:
    result += m[0][2] * det([m[1].xyw, m[2].xyw, m[3].xyw])
  if m[0][3] != 0:
    result -= m[0][3] * det([m[1].xyz, m[2].xyz, m[3].xyz])

func inverse*[T](m: Mat[2,2,T]): Mat[2,2,T]=
  # one over determinat

  let od = T(1) / det(m)

  result[0][0] =   m[1][1] * od
  result[0][1] = - m[0][1] * od
  result[1][0] = - m[1][0] * od
  result[1][1] =   m[0][0] * od

func inverse*[T](m: Mat[3,3,T]): Mat[3,3,T]=
  # one over determinant
  result = [
    [ (m[1][1] * m[2][2] - m[2][1] * m[1][2]),
     -(m[0][1] * m[2][2] - m[2][1] * m[0][2]),
      (m[0][1] * m[1][2] - m[1][1] * m[0][2])],

    [-(m[1][0] * m[2][2] - m[2][0] * m[1][2]),
      (m[0][0] * m[2][2] - m[2][0] * m[0][2]),
     -(m[0][0] * m[1][2] - m[1][0] * m[0][2])],

    [ (m[1][0] * m[2][1] - m[2][0] * m[1][1]),
     -(m[0][0] * m[2][1] - m[2][0] * m[0][1]),
      (m[0][0] * m[1][1] - m[1][0] * m[0][1])]]

  let d = det(m)
  for row in result.mitems:
    for x in row.mitems:
      x /= d

func inverse*[T](m: Mat[4,4,T]):Mat[4,4,T]=
  let
    Coef00:T = (m[2][2] * m[3][3]) - (m[3][2] * m[2][3])
    Coef02:T = (m[1][2] * m[3][3]) - (m[3][2] * m[1][3])
    Coef03:T = (m[1][2] * m[2][3]) - (m[2][2] * m[1][3])

    Coef04:T = (m[2][1] * m[3][3]) - (m[3][1] * m[2][3])
    Coef06:T = (m[1][1] * m[3][3]) - (m[3][1] * m[1][3])
    Coef07:T = (m[1][1] * m[2][3]) - (m[2][1] * m[1][3])

    Coef08:T = (m[2][1] * m[3][2]) - (m[3][1] * m[2][2])
    Coef10:T = (m[1][1] * m[3][2]) - (m[3][1] * m[1][2])
    Coef11:T = (m[1][1] * m[2][2]) - (m[2][1] * m[1][2])

    Coef12:T = (m[2][0] * m[3][3]) - (m[3][0] * m[2][3])
    Coef14:T = (m[1][0] * m[3][3]) - (m[3][0] * m[1][3])
    Coef15:T = (m[1][0] * m[2][3]) - (m[2][0] * m[1][3])

    Coef16:T = (m[2][0] * m[3][2]) - (m[3][0] * m[2][2])
    Coef18:T = (m[1][0] * m[3][2]) - (m[3][0] * m[1][2])
    Coef19:T = (m[1][0] * m[2][2]) - (m[2][0] * m[1][2])

    Coef20:T = (m[2][0] * m[3][1]) - (m[3][0] * m[2][1])
    Coef22:T = (m[1][0] * m[3][1]) - (m[3][0] * m[1][1])
    Coef23:T = (m[1][0] * m[2][1]) - (m[2][0] * m[1][1])

    Fac0 = [Coef00, Coef00, Coef02, Coef03]
    Fac1 = [Coef04, Coef04, Coef06, Coef07]
    Fac2 = [Coef08, Coef08, Coef10, Coef11]
    Fac3 = [Coef12, Coef12, Coef14, Coef15]
    Fac4 = [Coef16, Coef16, Coef18, Coef19]
    Fac5 = [Coef20, Coef20, Coef22, Coef23]

    array0 = [m[1,0], m[0,0], m[0,0], m[0,0]]
    array1 = [m[1,1], m[0,1], m[0,1], m[0,1]]
    array2 = [m[1,2], m[0,2], m[0,2], m[0,2]]
    array3 = [m[1,3], m[0,3], m[0,3], m[0,3]]

    Inv0 = (array1 * Fac0) - (array2 * Fac1) + (array3 * Fac2)
    Inv1 = (array0 * Fac0) - (array2 * Fac3) + (array3 * Fac4)
    Inv2 = (array0 * Fac1) - (array1 * Fac3) + (array3 * Fac5)
    Inv3 = (array0 * Fac2) - (array1 * Fac4) + (array2 * Fac5)

    SignA = [ 1d, -1,  1, -1]
    SignB = [-1d,  1, -1,  1]

    col0 = Inv0 * SignA
    col1 = Inv1 * SignB
    col2 = Inv2 * SignA
    col3 = Inv3 * SignB

    Inverse : Mat[4,4,T] = [col0, col1, col2, col3]

    Dot0 = m[0] * Inverse{0}
    Dot1 = (Dot0.x + Dot0.y) + (Dot0.z + Dot0.w)

  result = (Inverse, Dot1).fmap(a/b)

func transpose*[M,N,T](m: Mat[M,N,T]): Mat[N,M,T] =
  for i in 0 ..< N:
    result[i] = m{i}


template foreachZipImpl(name,op: untyped): untyped =
  func name*[M,N,T](m1,m2: Mat[M,N,T]): Mat[M,N,T] =
    for i in 0 ..< M:
      result.arr[i] = op(m1.arr[i], m2.arr[i])

foreachZipImpl(`+`,`+`)
foreachZipImpl(`-`,`-`)
foreachZipImpl(`.+`,`+`)
foreachZipImpl(`.-`,`-`)
foreachZipImpl(`.*`,`*`)
foreachZipImpl(`./`,`/`)
foreachZipImpl(matrixCompMult,`*`)
{.pop.}


# =====================================================================// Math #

when isMainModule:

  var mats : array[2, Mat[4,4,float32]]

  for m in mats.mitems:
    var x = 0
    for i in 0..3:
      for j in 0..3:
        m[i][j] = float32(x)
        x += 1

  echo mats[0]
  echo mats[1]

  echo mats[0] * mats[1]

  let m22 = [[1d, 5, 10], [0.66,1,70], [10d,2,1]]
  let m22i = inverse(m22)

  echo m22 * m22i

  let v2m = [2d,2,2] * m22
  discard v2m * m22i

  var m22v = m22
  m22v *= 3
  echo m22v

  echo mat3(5.0)

  echo det(diag([1,2,3,4]))

  let v0 = [3,0,5,6]
  let v1 = [7,2,4,6]
  let v2 = [3,-1,3,4]
  let v3 = [0,1,2,-1]

  let someMat = [v0,v1,v2,v3]

  echo "someMat:\n", someMat
  echo "det: ", someMat.det

  var mm : Mat[3,2,int]
  mm[1] = [33,33]

  echo mm

proc mix*[M,N,T](v1,v2: Mat[M,N,T]; a: T): Mat[M,N,T] =
  v1 * (1 - a) + v2 * a
