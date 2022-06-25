import std/macros

## Note:
## Expression is expanded as it is, but Args are copied once.
## It means that:

runnableExamples:
  var scalar = 0d
  var vector = 0d
  proc scalar_sideEffect(): float =
    scalar += 1
    scalar
  proc vector_sideEffect(): array[3,float] =
    vector += 1
    [vector].xxx

  proc `*`(v: array[3,float]; s: float): array[3,float] = v.fmap(a*s)

  doAssert vector_sideEffect() <$> a * scalar_sideEffect() == [1d, 2, 3]
  doAssert vector == 1
  doAssert scalar == 3

  doAssert vector_sideEffect() * scalar_sideEffect() == [8d, 8, 8]
  doAssert vector == 2
  doAssert scalar == 4

  let s = scalar_sideEffect()
  doAssert vector_sideEffect() <$> a * s == [15d, 15, 15]
  doAssert vector == 3
  doAssert scalar == 5

## If an expression with side effects is to be acted upon by fmap,
## it may be necessary to create variables or define wrapper functions
## in advance, depending on the purpose.

runnableExamples:
  import vkm/math
  import vkm/angles
  proc `*`(v: array[3,float]; s: float): array[3,float] = v.fmap(a*s)
  let v = [1d,2,3]

  block Continuous_Simple_Expression:
    # Note: Because the array is copied many times
    # faster:
    discard v <$> 0.001 * (0.5*a*a)
    # slower:
    discard 0.001 * (0.5*v*v)

  block Single_Heavy_Expression:
    # Note: Because the tangent is calculated many times
    # faster:
    discard v * (sin(30'rad)/cos(30'rad))
    # slower:
    discard v <$> a * (sin(30'rad)/cos(30'rad))

  # bench.nim                                       relative  time/iter  iters/s
  # ============================================================================
  # Continuous_Simple_Expression_faster                          8.58ns  116.49M
  # Continuous_Simple_Expression_slower                         74.26ns   13.47M
  # Single_Heavy_Expression_faster                             202.19ns    4.95M
  # Single_Heavy_Expression_slower                             569.18ns    1.76M

type Unit* = object
type Array*[I: static int] = array[I, Unit]

func replace(node: NimNode; src: NimNode; dst: NimNode): NimNode =
  if node.len == 0: return
    if node.eqIdent src: dst
    else: node

  result = node.kind.newNimNode()
  for n in node:
    result.add do:
      if n.eqIdent src: dst
      else: n.replace(src,dst)

macro fmap*[N; T](AT: typedesc[array[N, T]]; exp: untyped): untyped =
  result = nnkBracket.newNimNode()
  let low = AT.getType[1][1][1]
  let high = AT.getType[1][1][2]
  for i in low.intVal..high.intVal:
    result.add exp

macro fmap*[N; T](v: array[N, T]; exp: untyped): untyped =
  result = newStmtList()
  let low = v.getType[1][1]
  let high = v.getType[1][2]
  let v0 = genSym(nskLet, "arg0")
  result.add nnkLetSection.newTree(
    newIdentDefs(v0, newEmptyNode(), v),
  )
  var bracket = nnkBracket.newNimNode()
  for i in low.intVal..high.intVal:
    bracket.add exp
      .replace(ident"a", nnkBracketExpr.newTree(v0,newlit i))
  result.add bracket

macro fmap*[N; T,S](vs: (array[N,T], array[N,S]); exp: untyped): untyped =
  result = newStmtList()
  let low = vs[0].getType[1][1]
  let high = vs[0].getType[1][2]
  let v0 = genSym(nskLet, "arg0")
  let v1 = genSym(nskLet, "arg1")
  result.add nnkLetSection.newTree(
    newIdentDefs(v0, newEmptyNode(), vs[0]),
    newIdentDefs(v1, newEmptyNode(), vs[1]),
  )
  var bracket = nnkBracket.newNimNode()
  for i in low.intVal..high.intVal:
    bracket.add exp
      .replace(ident"a", nnkBracketExpr.newTree(v0,newlit i))
      .replace(ident"b", nnkBracketExpr.newTree(v1,newlit i))
  result.add bracket

macro fmap*[N; T,S](vs: (array[N,T], S); exp: untyped): untyped =
  result = newStmtList()
  let low = vs[0].getType[1][1]
  let high = vs[0].getType[1][2]
  let v0 = genSym(nskLet, "arg0")
  result.add nnkLetSection.newTree(
    newIdentDefs(v0, newEmptyNode(), vs[0]),
  )
  var bracket = nnkBracket.newNimNode()
  for i in low.intVal..high.intVal:
    bracket.add exp
      .replace(ident"a", nnkBracketExpr.newTree(v0,newlit i))
      .replace(ident"b", vs[1])
  result.add bracket

macro fmap*[N; T,S](vs: (T, array[N,S]); exp: untyped): untyped =
  result = newStmtList()
  let low = vs[1].getType[1][1]
  let high = vs[1].getType[1][2]
  let v1 = genSym(nskLet, "arg1")
  result.add nnkLetSection.newTree(
    newIdentDefs(v1, newEmptyNode(), vs[1]),
  )
  var bracket = nnkBracket.newNimNode()
  for i in low.intVal..high.intVal:
    bracket.add exp
      .replace(ident"a", vs[0])
      .replace(ident"b", nnkBracketExpr.newTree(v1,newlit i))
  result.add bracket

macro fmap*[N; T,S,R](vs: (array[N,T], array[N,S], array[N,R]); exp: untyped): untyped =
  result = newStmtList()
  let low = vs[0].getType[1][1]
  let high = vs[0].getType[1][2]
  let v0 = genSym(nskLet, "arg0")
  let v1 = genSym(nskLet, "arg1")
  let v2 = genSym(nskLet, "arg2")
  result.add nnkLetSection.newTree(
    newIdentDefs(v0, newEmptyNode(), vs[0]),
    newIdentDefs(v1, newEmptyNode(), vs[1]),
    newIdentDefs(v2, newEmptyNode(), vs[2]),
  )
  var bracket = nnkBracket.newNimNode()
  for i in low.intVal..high.intVal:
    bracket.add exp
      .replace(ident"a", nnkBracketExpr.newTree(v0,newlit i))
      .replace(ident"b", nnkBracketExpr.newTree(v1,newlit i))
      .replace(ident"c", nnkBracketExpr.newTree(v2,newlit i))
  result.add bracket