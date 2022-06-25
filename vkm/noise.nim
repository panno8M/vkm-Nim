import ./math
import ./detail
import ./vectors
import ./functionalops
import ./details/interpolation

# Based on the work of Stefan Gustavson and Ashima Arts on "webgl-noise":
# https://github.com/ashima/webgl-noise
# Following Stefan Gustavson's paper "Simplex noise demystified":
# http://www.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf

func gtc_grad4[T](j: T; ip: array[4,T]): array[4,T] =
  var pXYZ = ip.xyz <$> (floor(fract(a*j)*T(7))*ip.z - T(1))
  let pW: T = T(1.5) - sum pXYZ <$> abs a
  let s = vec(pXYZ, pW) <$> T(a < 0)
  pXYZ = (pXYZ, s.xyz) <$> a + (b*T(2) - T(1)) * s.w
  return vec(pXYZ, pW)

func perlin*[T](pos: array[2,T]): T =
  ## Clissic Perlin noise
  var Pi = (pos.xyxy, [T(0),0,1,1]) <$> floor(a)+b
  let Pf = (pos.xyxy, [T(0),0,1,1]) <$> fract(a)-b
  Pi = Pi <$> floorMod(a,T(289)) # To avoid truncation effects in permutation
  let ix = Pi.xzxz
  let iy = Pi.yyww
  let fx = Pf.xzxz
  let fy = Pf.yyww

  let i = (ix, iy) <$> permute(permute(a) + b)

  var gx = i <$> T(2) * fract(a/T(41)) - T(1)
  let gy = gx <$> abs(a) - T(0.5)
  let tx = gx <$> floor a+T(0.5)
  gx = (gx, tx) <$> a-b

  var g = [
    [gx.x, gy.x],
    [gx.z, gy.z],
    [gx.y, gy.y],
    [gx.w, gy.w]]

  let norm = g <$> a.norm2.taylorInvSqrt
  g.x = (g.x, norm.x).fmap(a*b)
  g.y = (g.y, norm.y).fmap(a*b)
  g.z = (g.z, norm.z).fmap(a*b)
  g.w = (g.w, norm.w).fmap(a*b)

  let n = [
    dot(g.x, [fx.x, fy.x]),
    dot(g.y, [fx.z, fy.z]),
    dot(g.z, [fx.y, fy.y]),
    dot(g.w, [fx.w, fy.w])]

  let fade_xy = Pf.xy <$> a.fade
  let n_x = (n.xy,n.zw) <$> lerp(a,b, fade_xy.x)
  let n_xy: T = lerp(n_x.x, n_x.y, fade_xy.y)
  return T(2.3) * n_xy

func perlin*[T](Position: array[3,T]): T =
  ## Classic Perlin noise

  var Pi0 = Position <$> floor a # Integer part for indexing
  var Pi1 = Pi0 <$> floorMod(a+T(1), 289) # Integer part + 1
  Pi0 = Pi0 <$> floorMod(a, 289)
  let Pf0 = Position <$> fract a # Fractional part for interpolation
  let Pf1 = Pf0 <$> a-T(1) # Fractional part - 1.0
  let ix = [Pi0.x, Pi1.x, Pi0.x, Pi1.x]
  let iy = vec(Pi0.yy, Pi1.yy)

  let ixy = (ix, iy) <$> permute(permute(a) + b)
  let ixy0 = ixy <$> permute(a + Pi0.z)
  let ixy1 = ixy <$> permute(a + Pi1.z)

  var gx0 = ixy0 <$> a/T(7)
  var gy0 = gx0 <$> fract(floor(a)/T(7)) - T(0.5)
  gx0 = gx0 <$> fract a
  let gz0 = (gx0,gy0) <$> T(0.5) - abs(a) - abs(b)
  let sz0 = gz0 <$> step(a, 0)
  gx0 = (gx0, sz0) <$> a - (step(T(0), a) - T(0.5)) * b
  gy0 = (gy0, sz0) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx1 = ixy1 <$> a/7.T
  var gy1 = gx1 <$> fract(floor(a) / T(7)) - T(0.5)
  gx1 = gx1 <$> fract a
  let gz1 = (gx1,gy1) <$> T(0.5) - abs(a) - abs(b)
  let sz1 = gz1 <$> step(a, 0)
  gx1 = (gx1, sz1) <$> a - (step(T(0), a) - T(0.5)) * b
  gy1 = (gy1, sz1) <$> a - (step(T(0), a) - T(0.5)) * b


  var g000 = [gx0.x, gy0.x, gz0.x]
  var g100 = [gx0.y, gy0.y, gz0.y]
  var g010 = [gx0.z, gy0.z, gz0.z]
  var g110 = [gx0.w, gy0.w, gz0.w]
  var g001 = [gx1.x, gy1.x, gz1.x]
  var g101 = [gx1.y, gy1.y, gz1.y]
  var g011 = [gx1.z, gy1.z, gz1.z]
  var g111 = [gx1.w, gy1.w, gz1.w]

  let norm0 = [g000,g010,g100,g110] <$> a.norm2.taylorInvSqrt
  g000 = (g000, norm0.x) <$> a*b
  g010 = (g010, norm0.y) <$> a*b
  g100 = (g100, norm0.z) <$> a*b
  g110 = (g110, norm0.w) <$> a*b
  let norm1 = [g001,g011,g101,g111] <$> a.norm2.taylorInvSqrt
  g001 = (g001, norm1.x) <$> a*b
  g011 = (g011, norm1.y) <$> a*b
  g101 = (g101, norm1.z) <$> a*b
  g111 = (g111, norm1.w) <$> a*b

  let n000: T = dot(g000, Pf0)
  let n100: T = dot(g100, [Pf1.x, Pf0.y, Pf0.z])
  let n010: T = dot(g010, [Pf0.x, Pf1.y, Pf0.z])
  let n110: T = dot(g110, [Pf1.x, Pf1.y, Pf0.z])
  let n001: T = dot(g001, [Pf0.x, Pf0.y, Pf1.z])
  let n101: T = dot(g101, [Pf1.x, Pf0.y, Pf1.z])
  let n011: T = dot(g011, [Pf0.x, Pf1.y, Pf1.z])
  let n111: T = dot(g111, Pf1)

  let fade_xyz = Pf0 <$> a.fade
  let n_z = ([n000, n100, n010, n110], [n001, n101, n011, n111]) <$> lerp(a,b, fade_xyz.z)
  let n_yz = (n_z.xy, n_z.zw) <$> lerp(a,b, fade_xyz.y)
  let n_xyz = lerp(n_yz.x, n_yz.y, fade_xyz.x)
  return T(2.2) * n_xyz

proc perlin*[T](Position: array[4,T]): T =
  ## Classic Perlin noise

  var Pi0 = Position <$> floor a  # Integer part for indexing
  var Pi1 = Pi0 <$> a+T(1)    # Integer part + 1
  Pi0 = Pi0 <$> floorMod(a, T(289))
  Pi1 = Pi1 <$> floorMod(a, T(289))
  let Pf0 = Position <$> fract a  # Fractional part for interpolation
  let Pf1 = Pf0 <$> a-T(1)    # Fractional part - 1.0
  let ix = [Pi0.x, Pi1.x, Pi0.x, Pi1.x]
  let iy = [Pi0.y, Pi0.y, Pi1.y, Pi1.y]

  let ixy = (ix,iy) <$> permute(permute(a)+b)
  let ixy0 = ixy <$> permute(a+Pi0.z)
  let ixy1 = ixy <$> permute(a+Pi1.z)
  let ixy00 = ixy0 <$> permute(a+Pi0.w)
  let ixy01 = ixy0 <$> permute(a+Pi1.w)
  let ixy10 = ixy1 <$> permute(a+Pi0.w)
  let ixy11 = ixy1 <$> permute(a+Pi1.w)

  var gx00 = ixy00 <$> a/T(7)
  var gy00 = gx00 <$> floor(a)/T(7)
  var gz00 = gy00 <$> floor(a)/T(6)
  gx00 = gx00 <$> fract(a) - 0.5.T
  gy00 = gy00 <$> fract(a) - 0.5.T
  gz00 = gz00 <$> fract(a) - 0.5.T
  let gw00 = (gx00,gy00,gz00) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw00 = gw00 <$> step(a, 0)
  gx00 = (gx00, sw00) <$> a - (step(T(0), a) - T(0.5)) * b
  gy00 = (gy00, sw00) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx01 = ixy01 <$> a/T(7)
  var gy01 = gx01 <$> floor(a)/T(7)
  var gz01 = gy01 <$> floor(a)/T(6)
  gx01 = gx01 <$> fract(a) - T(0.5)
  gy01 = gy01 <$> fract(a) - T(0.5)
  gz01 = gz01 <$> fract(a) - T(0.5)
  var gw01 = (gx01,gy01,gz01) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  var sw01 = gw01 <$> step(a, 0)
  gx01 = (gx01, sw01) <$> a - (step(T(0), a) - T(0.5)) * b
  gy01 = (gy01, sw01) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx10 = ixy10 <$> a/T(7)
  var gy10 = gx10 <$> floor(a)/T(7)
  var gz10 = gy10 <$> floor(a)/T(6)
  gx10 = gx10 <$> fract(a) - T(0.5)
  gy10 = gy10 <$> fract(a) - T(0.5)
  gz10 = gz10 <$> fract(a) - T(0.5)
  let gw10 = (gx10,gy10,gz10) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw10 = gw10 <$> step(a, 0)
  gx10 = (gx10, sw10) <$> a - (step(T(0), a) - T(0.5)) * b
  gy10 = (gy10, sw10) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx11 = ixy11 <$> a/T(7)
  var gy11 = gx11 <$> floor(a)/T(7)
  var gz11 = gy11 <$> floor(a)/T(6)
  gx11 = gx11 <$> fract(a) - T(0.5)
  gy11 = gy11 <$> fract(a) - T(0.5)
  gz11 = gz11 <$> fract(a) - T(0.5)
  let gw11 = (gx11,gy11,gz11) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw11 = gw11 <$> step(a, 0)
  gx11 = (gx11, sw11) <$> a - (step(T(0), a) - T(0.5)) * b
  gy11 = (gy11, sw11) <$> a - (step(T(0), a) - T(0.5)) * b

  var
    g0000 = [gx00.x, gy00.x, gz00.x, gw00.x]
    g1000 = [gx00.y, gy00.y, gz00.y, gw00.y]
    g0100 = [gx00.z, gy00.z, gz00.z, gw00.z]
    g1100 = [gx00.w, gy00.w, gz00.w, gw00.w]
    g0010 = [gx10.x, gy10.x, gz10.x, gw10.x]
    g1010 = [gx10.y, gy10.y, gz10.y, gw10.y]
    g0110 = [gx10.z, gy10.z, gz10.z, gw10.z]
    g1110 = [gx10.w, gy10.w, gz10.w, gw10.w]
    g0001 = [gx01.x, gy01.x, gz01.x, gw01.x]
    g1001 = [gx01.y, gy01.y, gz01.y, gw01.y]
    g0101 = [gx01.z, gy01.z, gz01.z, gw01.z]
    g1101 = [gx01.w, gy01.w, gz01.w, gw01.w]
    g0011 = [gx11.x, gy11.x, gz11.x, gw11.x]
    g1011 = [gx11.y, gy11.y, gz11.y, gw11.y]
    g0111 = [gx11.z, gy11.z, gz11.z, gw11.z]
    g1111 = [gx11.w, gy11.w, gz11.w, gw11.w]

  let norm00 = [g0000, g0100, g1000, g1100] <$> a.norm2.taylorInvSqrt
  g0000 = (g0000,norm00.x) <$> a*b
  g0100 = (g0100,norm00.y) <$> a*b
  g1000 = (g1000,norm00.z) <$> a*b
  g1100 = (g1100,norm00.w) <$> a*b

  let norm01 = [g0001, g0101, g1001, g1101] <$> a.norm2.taylorInvSqrt
  g0001 = (g0001,norm01.x) <$> a*b
  g0101 = (g0101,norm01.y) <$> a*b
  g1001 = (g1001,norm01.z) <$> a*b
  g1101 = (g1101,norm01.w) <$> a*b

  let norm10 = [g0010, g0110, g1010, g1110] <$> a.norm2.taylorInvSqrt
  g0010 = (g0010,norm10.x) <$> a*b
  g0110 = (g0110,norm10.y) <$> a*b
  g1010 = (g1010,norm10.z) <$> a*b
  g1110 = (g1110,norm10.w) <$> a*b

  let norm11 = [g0011, g0111, g1011, g1111] <$> a.norm2.taylorInvSqrt
  g0011 = (g0011,norm11.x) <$> a*b
  g0111 = (g0111,norm11.y) <$> a*b
  g1011 = (g1011,norm11.z) <$> a*b
  g1111 = (g1111,norm11.w) <$> a*b

  let n0000: T = dot(g0000, Pf0)
  let n1000: T = dot(g1000, [Pf1.x, Pf0.y, Pf0.z, Pf0.w])
  let n0100: T = dot(g0100, [Pf0.x, Pf1.y, Pf0.z, Pf0.w])
  let n1100: T = dot(g1100, [Pf1.x, Pf1.y, Pf0.z, Pf0.w])
  let n0010: T = dot(g0010, [Pf0.x, Pf0.y, Pf1.z, Pf0.w])
  let n1010: T = dot(g1010, [Pf1.x, Pf0.y, Pf1.z, Pf0.w])
  let n0110: T = dot(g0110, [Pf0.x, Pf1.y, Pf1.z, Pf0.w])
  let n1110: T = dot(g1110, [Pf1.x, Pf1.y, Pf1.z, Pf0.w])
  let n0001: T = dot(g0001, [Pf0.x, Pf0.y, Pf0.z, Pf1.w])
  let n1001: T = dot(g1001, [Pf1.x, Pf0.y, Pf0.z, Pf1.w])
  let n0101: T = dot(g0101, [Pf0.x, Pf1.y, Pf0.z, Pf1.w])
  let n1101: T = dot(g1101, [Pf1.x, Pf1.y, Pf0.z, Pf1.w])
  let n0011: T = dot(g0011, [Pf0.x, Pf0.y, Pf1.z, Pf1.w])
  let n1011: T = dot(g1011, [Pf1.x, Pf0.y, Pf1.z, Pf1.w])
  let n0111: T = dot(g0111, [Pf0.x, Pf1.y, Pf1.z, Pf1.w])
  let n1111: T = dot(g1111, Pf1)

  let fade_xyzw = Pf0 <$> a.fade
  let n_0w = ([n0000, n1000, n0100, n1100], [n0001, n1001, n0101, n1101]) <$> lerp(a,b, fade_xyzw.w)
  let n_1w = ([n0010, n1010, n0110, n1110], [n0011, n1011, n0111, n1111]) <$> lerp(a,b, fade_xyzw.w)
  let n_zw = (n_0w, n_1w) <$> lerp(a,b, fade_xyzw.z)
  let n_yzw = (n_zw.xy, n_zw.zw) <$> lerp(a,b, fade_xyzw.y)
  let n_xyzw: T = lerp(n_yzw.x, n_yzw.y, fade_xyzw.x)
  return n_xyzw * T(2.2)


# Classic Perlin noise, periodic variant
proc perlin*[T](Position: array[2,T]; rep: array[2,T]): T =

  var Pi = (Position.xyxy, [T(0), 0, 1, 1]) <$> floor(a)+b
  let Pf = (Position.xyxy, [T(0), 0, 1, 1]) <$> fract(a)-b
  Pi = (Pi, rep.xyxy) <$> floorMod(a, b) # To create noise with explicit period
  Pi = Pi <$> floorMod(a, 289.T) # To avoid truncation effects in permutation
  let ix = Pi.xzxz
  let iy = Pi.yyww
  let fx = Pf.xzxz
  let fy = Pf.yyww

  let i = (ix,iy) <$> permute(permute(a) + b)

  var gx = i <$> T(2) * fract(a/T(41)) - T(1)
  let gy = gx <$> abs(a) - T(0.5)
  let tx = gx <$> floor a + T(0.5)
  gx = (gx, tx) <$> a-b

  var g = [
    [gx.x, gy.x],
    [gx.z, gy.z],
    [gx.y, gy.y],
    [gx.w, gy.w]]

  let norm = g <$> a.norm2.taylorInvSqrt
  g.x = (g.x, norm.x) <$> a*b
  g.y = (g.y, norm.y) <$> a*b
  g.z = (g.z, norm.z) <$> a*b
  g.w = (g.w, norm.w) <$> a*b

  let n = ([
    [fx.x, fy.x],
    [fx.z, fy.z],
    [fx.y, fy.y],
    [fx.w, fy.w]], g) <$> dot(a,b)

  let fade_xy = Pf.xy <$> a.fade
  let n_x = (n.xy, n.zw) <$> lerp(a,b, fade_xy.x)
  let n_xy: T = lerp(n_x.x, n_x.y, fade_xy.y)
  return T(2.3) * n_xy


proc perlin*[T](Position: array[3,T]; rep: array[3,T]): T =
  ## Classic Perlin noise, periodic variant

  var Pi0 = (Position,rep) <$> floorMod(floor(a), b) # Integer part, modulo period
  var Pi1 = (Pi0,rep) <$> floorMod(a+1.T, b) # Integer part + 1, mod period
  Pi0 = Pi0 <$> floorMod(a, T(289))
  Pi1 = Pi1 <$> floorMod(a, T(289))
  let Pf0 = Position <$> fract a # Fractional part for interpolation
  let Pf1 = Pf0 <$> a-T(1) # Fractional part - 1.0
  let ix = [Pi0.x, Pi1.x, Pi0.x, Pi1.x]
  let iy = [Pi0.y, Pi0.y, Pi1.y, Pi1.y]

  let ixy = (ix,iy) <$> permute(permute(a)+b)
  let ixy0 = ixy <$> permute(a+Pi0.z)
  let ixy1 = ixy <$> permute(a+Pi1.z)

  var gx0 = ixy0 <$> a/T(7)
  var gy0 = gx0 <$> fract(floor(a)/T(7)) - T(0.5)
  gx0 = gx0 <$> fract a
  let gz0 = (gx0,gy0) <$> T(0.5)-abs(a)-abs(b)
  let sz0 = gz0 <$> step(a, T(0))
  gx0 = (gx0, sz0) <$> a - (step(T(0), a) - T(0.5)) * b
  gy0 = (gy0, sz0) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx1 = ixy1 <$> a/T(7)
  var gy1 = gx1 <$> fract(floor(a)/T(7)) - T(0.5)
  gx1 = gx1 <$> fract(a)
  let gz1 = (gx1,gy1) <$> T(0.5)-abs(a)-abs(b)
  let sz1 = gz1 <$> step(a, T(0))
  gx1 = (gx1, sz1) <$> a - (step(T(0), a) - T(0.5)) * b
  gy1 = (gy1, sz1) <$> a - (step(T(0), a) - T(0.5)) * b

  var g000 = [gx0.x, gy0.x, gz0.x]
  var g100 = [gx0.y, gy0.y, gz0.y]
  var g010 = [gx0.z, gy0.z, gz0.z]
  var g110 = [gx0.w, gy0.w, gz0.w]
  var g001 = [gx1.x, gy1.x, gz1.x]
  var g101 = [gx1.y, gy1.y, gz1.y]
  var g011 = [gx1.z, gy1.z, gz1.z]
  var g111 = [gx1.w, gy1.w, gz1.w]

  let norm0 = [g000,g010,g100,g110] <$> a.norm2.taylorInvSqrt
  g000 = (g000,norm0.x) <$> a*b
  g010 = (g010,norm0.y) <$> a*b
  g100 = (g100,norm0.z) <$> a*b
  g110 = (g110,norm0.w) <$> a*b
  let norm1 = [g001,g011,g101,g111] <$> a.norm2.taylorInvSqrt
  g001 = (g001,norm1.x) <$> a*b
  g011 = (g011,norm1.y) <$> a*b
  g101 = (g101,norm1.z) <$> a*b
  g111 = (g111,norm1.w) <$> a*b

  let n000 = dot(g000, Pf0)
  let n100 = dot(g100, [Pf1.x, Pf0.y, Pf0.z])
  let n010 = dot(g010, [Pf0.x, Pf1.y, Pf0.z])
  let n110 = dot(g110, [Pf1.x, Pf1.y, Pf0.z])
  let n001 = dot(g001, [Pf0.x, Pf0.y, Pf1.z])
  let n101 = dot(g101, [Pf1.x, Pf0.y, Pf1.z])
  let n011 = dot(g011, [Pf0.x, Pf1.y, Pf1.z])
  let n111 = dot(g111, Pf1)

  let fade_xyz = Pf0 <$> a.fade
  let n_z = ([n000, n100, n010, n110], [n001, n101, n011, n111]) <$> lerp(a,b, fade_xyz.z)
  let n_yz = (n_z.xy, n_z.zw) <$> lerp(a,b, fade_xyz.y)
  let n_xyz = lerp(n_yz.x, n_yz.y, fade_xyz.x)
  return T(2.2) * n_xyz

proc perlin*[T](Position: array[4,T]; rep: array[4,T]): T =
  ## Classic Perlin noise, periodic version

  let Pi0 = (Position,rep) <$> floorMod(floor(a), b) # Integer part modulo rep
  let Pi1 = (Pi0,rep) <$> floorMod(a+T(1), b) # Integer part + 1 mod rep
  let Pf0 = Position <$> fract(a) # Fractional part for interpolation
  let Pf1 = Pf0 <$> a-T(1) # Fractional part - 1.0
  let ix = [Pi0.x, Pi1.x, Pi0.x, Pi1.x]
  let iy = [Pi0.y, Pi0.y, Pi1.y, Pi1.y]

  let ixy = (ix,iy) <$> permute(permute(a)+b)
  let ixy0 = ixy <$> permute(a+Pi0.z)
  let ixy1 = ixy <$> permute(a+Pi1.z)
  let ixy00 = ixy0 <$> permute(a+Pi0.w)
  let ixy01 = ixy0 <$> permute(a+Pi1.w)
  let ixy10 = ixy1 <$> permute(a+Pi0.w)
  let ixy11 = ixy1 <$> permute(a+Pi1.w)

  var gx00 = ixy00 <$> a/T(7)
  var gy00 = gx00 <$> floor(a)/T(7)
  var gz00 = gy00 <$> floor(a)/T(6)
  gx00 = gx00 <$> fract(a) - T(0.5)
  gy00 = gy00 <$> fract(a) - T(0.5)
  gz00 = gz00 <$> fract(a) - T(0.5)
  let gw00 = (gx00,gy00,gz00) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw00 = gw00 <$> step(a, T(0))
  gx00 = (gx00, sw00) <$> a - (step(T(0), a) - T(0.5)) * b
  gy00 = (gy00, sw00) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx01 = ixy01 <$> a/T(7)
  var gy01 = gx01 <$> floor(a)/T(7)
  var gz01 = gy01 <$> floor(a)/T(6)
  gx01 = gx01 <$> fract(a) - T(0.5)
  gy01 = gy01 <$> fract(a) - T(0.5)
  gz01 = gz01 <$> fract(a) - T(0.5)
  let gw01 = (gx01,gy01,gz01) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw01 = gw01 <$> step(a, T(0))
  gx01 = (gx01, sw01) <$> a - (step(T(0), a) - T(0.5)) * b
  gy01 = (gy01, sw01) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx10 = ixy10 <$> a/T(7)
  var gy10 = gx10 <$> floor(a)/T(7)
  var gz10 = gy10 <$> floor(a)/T(6)
  gx10 = gx10 <$> fract(a) - T(0.5)
  gy10 = gy10 <$> fract(a) - T(0.5)
  gz10 = gz10 <$> fract(a) - T(0.5)
  let gw10 = (gx10,gy10,gz10) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw10 = gw10 <$> step(a, T(0))
  gx10 = (gx10, sw10) <$> a - (step(T(0), a) - T(0.5)) * b
  gy10 = (gy10, sw10) <$> a - (step(T(0), a) - T(0.5)) * b

  var gx11 = ixy11 <$> a/T(7)
  var gy11 = gx11 <$> floor(a)/T(7)
  var gz11 = gy11 <$> floor(a)/T(6)
  gx11 = gx11 <$> fract(a) - T(0.5)
  gy11 = gy11 <$> fract(a) - T(0.5)
  gz11 = gz11 <$> fract(a) - T(0.5)
  let gw11 = (gx11,gy11,gz11) <$> T(0.75)-abs(a)-abs(b)-abs(c)
  let sw11 = gw11 <$> step(a, T(0))
  gx11 = (gx11, sw11) <$> a - (step(T(0), a) - T(0.5)) * b
  gy11 = (gy11, sw11) <$> a - (step(T(0), a) - T(0.5)) * b

  var g0000 = [gx00.x, gy00.x, gz00.x, gw00.x]
  var g1000 = [gx00.y, gy00.y, gz00.y, gw00.y]
  var g0100 = [gx00.z, gy00.z, gz00.z, gw00.z]
  var g1100 = [gx00.w, gy00.w, gz00.w, gw00.w]
  var g0010 = [gx10.x, gy10.x, gz10.x, gw10.x]
  var g1010 = [gx10.y, gy10.y, gz10.y, gw10.y]
  var g0110 = [gx10.z, gy10.z, gz10.z, gw10.z]
  var g1110 = [gx10.w, gy10.w, gz10.w, gw10.w]
  var g0001 = [gx01.x, gy01.x, gz01.x, gw01.x]
  var g1001 = [gx01.y, gy01.y, gz01.y, gw01.y]
  var g0101 = [gx01.z, gy01.z, gz01.z, gw01.z]
  var g1101 = [gx01.w, gy01.w, gz01.w, gw01.w]
  var g0011 = [gx11.x, gy11.x, gz11.x, gw11.x]
  var g1011 = [gx11.y, gy11.y, gz11.y, gw11.y]
  var g0111 = [gx11.z, gy11.z, gz11.z, gw11.z]
  var g1111 = [gx11.w, gy11.w, gz11.w, gw11.w]

  let norm00 = [g0000,g0100,g1000,g1100] <$> a.norm2.taylorInvSqrt
  g0000 = (g0000,norm00.x) <$> a*b
  g0100 = (g0100,norm00.y) <$> a*b
  g1000 = (g1000,norm00.z) <$> a*b
  g1100 = (g1100,norm00.w) <$> a*b

  let norm01 = [g0001,g0101,g1001,g1101] <$> a.norm2.taylorInvSqrt
  g0001 = (g0001,norm01.x) <$> a*b
  g0101 = (g0101,norm01.y) <$> a*b
  g1001 = (g1001,norm01.z) <$> a*b
  g1101 = (g1101,norm01.w) <$> a*b

  let norm10 = [g0010,g0110,g1010,g1110] <$> a.norm2.taylorInvSqrt
  g0010 = (g0010,norm10.x) <$> a*b
  g0110 = (g0110,norm10.y) <$> a*b
  g1010 = (g1010,norm10.z) <$> a*b
  g1110 = (g1110,norm10.w) <$> a*b

  let norm11 = [g0011,g0111,g1011,g1111] <$> a.norm2.taylorInvSqrt
  g0011 = (g0011,norm11.x) <$> a*b
  g0111 = (g0111,norm11.y) <$> a*b
  g1011 = (g1011,norm11.z) <$> a*b
  g1111 = (g1111,norm11.w) <$> a*b

  let n0000: T = dot(g0000, Pf0)
  let n0001: T = dot(g0001, [Pf0.x, Pf0.y, Pf0.z, Pf1.w])
  let n0010: T = dot(g0010, [Pf0.x, Pf0.y, Pf1.z, Pf0.w])
  let n0011: T = dot(g0011, [Pf0.x, Pf0.y, Pf1.z, Pf1.w])
  let n0100: T = dot(g0100, [Pf0.x, Pf1.y, Pf0.z, Pf0.w])
  let n0101: T = dot(g0101, [Pf0.x, Pf1.y, Pf0.z, Pf1.w])
  let n0110: T = dot(g0110, [Pf0.x, Pf1.y, Pf1.z, Pf0.w])
  let n0111: T = dot(g0111, [Pf0.x, Pf1.y, Pf1.z, Pf1.w])
  let n1000: T = dot(g1000, [Pf1.x, Pf0.y, Pf0.z, Pf0.w])
  let n1001: T = dot(g1001, [Pf1.x, Pf0.y, Pf0.z, Pf1.w])
  let n1010: T = dot(g1010, [Pf1.x, Pf0.y, Pf1.z, Pf0.w])
  let n1011: T = dot(g1011, [Pf1.x, Pf0.y, Pf1.z, Pf1.w])
  let n1100: T = dot(g1100, [Pf1.x, Pf1.y, Pf0.z, Pf0.w])
  let n1101: T = dot(g1101, [Pf1.x, Pf1.y, Pf0.z, Pf1.w])
  let n1110: T = dot(g1110, [Pf1.x, Pf1.y, Pf1.z, Pf0.w])
  let n1111: T = dot(g1111, Pf1)

  let fade_xyzw = Pf0 <$> a.fade
  let n_0w = ([n0000, n1000, n0100, n1100], [n0001, n1001, n0101, n1101]) <$> lerp(a,b, fade_xyzw.w)
  let n_1w = ([n0010, n1010, n0110, n1110], [n0011, n1011, n0111, n1111]) <$> lerp(a,b, fade_xyzw.w)
  let n_zw = (n_0w, n_1w) <$> lerp(a,b, fade_xyzw.z)
  let n_yzw = (n_zw.xy, n_zw.zw) <$> lerp(a,b, fade_xyzw.y)
  let n_xyzw: T = lerp(n_yzw.x, n_yzw.y, fade_xyzw.x)
  return T(2.2) * n_xyzw


proc simplex*[T](v: array[2,T]): T =

  let
    C = [
      T( 0.211324865405187),  # (3.0 -  sqrt(3.0)) / 6.0
      T( 0.366025403784439),  #  0.5 * (sqrt(3.0)  - 1.0)
      T(-0.577350269189626),  # -1.0 + 2.0 * C.x
      T( 0.024390243902439)]  #  1.0 / 41.0

  # First corner

  var i  = v <$> floor(a + dot(v, C.yy))
  let x0 = (v,i) <$> a - b + dot(i, C.xx)

  # Other corners
  #i1.x = step( x0.y, x0.x ) # x0.x ] x0.y ? 1.0 : 0.0
  #i1.y = 1.0 - i1.x
  let i1 = if x0.x > x0.y: [T(1), 0] else: [T(0), 1]
  # x0 = x0 - 0.0 + 0.0 * C.xx
  # x1 = x0 - i1 + 1.0 * C.xx
  # x2 = x0 - 1.0 + 2.0 * C.xx
  var x12 = x0.xyxy + C.xxzz
  x12 = vec(x12.xy - i1, x12.z, x12.w)

  # Permutations
  i = floorMod(i, [T(289)].xx) # Avoid truncation effects in permutation
  let py = [T(0), i1.y, 1] <$> permute a + i.y
  let p = ( py, [T(0), i1.x, 1]) <$> permute a + b + i.x

  var m = max([T(0.5)].xxx - [x0, x12.xy, x12.zw].fmap(a.norm2), [T(0)].xxx)
  m = m * m
  m = m * m

  # Gradients: 41 points uniformly over a line, mapped onto a diamond.
  # The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)

  let x = T(2) * fract(p * C.w) - T(1)
  let h = abs(x) - T(0.5)
  let ox = floor(x + T(0.5))
  let a0 = x - ox

  # Normalise gradients implicitly by scaling m
  # Inlined for speed: m *= taylorInvSqrt( a0*a0 + h*h )
  m *= T(1.79284291400159) - T(0.85373472095314) * (a0 * a0 + h * h)

  # Compute final noise value at P
  let g = [
    a0.x  * x0.x + h.x *  x0.y,
    a0.y * x12.x + h.y * x12.y,
    a0.z * x12.z + h.z * x12.w]
  return T(130) * dot(m, g)


import macros

proc simplex*[T](v: array[3,T]): T =
  when T is float64:
    {.warning: "simplex with argument of type Vec3d is broken".}


  let
    C = [T(6),3] <$> (1/a)
    D = [T(0),0.5,1,2]

  # First corner
  var i = floor(v + dot(v, C.yyy))
  let x0 = v - i + dot(i, C.xxx)

  # Other corners
  let g = step(x0.yzx, x0)
  let l = T(1) - g
  let i1 = min(g, l.zxy)
  let i2 = max(g, l.zxy)

  #   x0 = x0 - 0.0 + 0.0 * C.xxx
  #   x1 = x0 - i1  + 1.0 * C.xxx
  #   x2 = x0 - i2  + 2.0 * C.xxx
  #   x3 = x0 - 1.0 + 3.0 * C.xxx
  let x1 = x0 - i1 + C.x
  let x2 = x0 - i2 + C.y # 2.0*C.x = 1/3 = C.y
  let x3 = x0 - D.y      # -1.0+3.0*C.x = -0.5 = -D.y

  # Permutations
  i = floorMod(i, 289)
  let
    pz = [T(0), i1.z, i2.z, 1] <$> permute a + i.z
    py = ([T(0), i1.y, i2.y, 1],pz) <$> permute a + b + i.y
    p = ([T(0), i1.x, i2.x, 1],py) <$> permute a + b + i.x

  # Gradients: 7x7 points over a square, mapped onto an octahedron.
  # The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  let n: T = 0.142857142857 # 1.0/7.0
  let ns = n * D.wyz - D.xzx

  let j = p - T(49) * floor(p * ns.z * ns.z)  #  modulo(p,7*7)

  let x_u = floor(j * ns.z)
  let y_u = floor(j - T(7) * x_u)    # modulo(j,N)

  let x = x_u * ns.x + ns.y
  let y = y_u * ns.x + ns.y
  let h = T(1) - abs(x) - abs(y)

  let b0 = vec(x.xy, y.xy)
  let b1 = vec(x.zw, y.zw)

  # vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0
  # vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0
  let s0 = floor(b0) * T(2) + T(1)
  let s1 = floor(b1) * T(2) + T(1)
  let sh = -step(h, [T(0)].xxxx)

  let a0 = b0.xzyw + s0.xzyw * sh.xxyy
  let a1 = b1.xzyw + s1.xzyw * sh.zzww

  var p0 = vec(a0.xy, h.x)
  var p1 = vec(a0.zw, h.y)
  var p2 = vec(a1.xy, h.z)
  var p3 = vec(a1.zw, h.w)

  # Normalise gradients
  let norm = [p0,p1,p2,p3] <$> a.norm2.taylorInvSqrt
  p0 *= norm.x
  p1 *= norm.y
  p2 *= norm.z
  p3 *= norm.w

  # Mix final noise value
  var m = max(T(0.6) - [x0,x1,x2,x3].fmap(a.norm2), [T(0)].xxxx)
  m = m * m
  return T(42) * dot(m * m, ([p0,p1,p2,p3],[x0,x1,x2,x3])<$>dot(a,b))


proc simplex*[T](v: array[4,T]): T =

  let
    C = [
      T(0.138196601125011), # (5 - sqrt(5))/20  G4
      0.276393202250021,    # 2 * G4
      0.414589803375032,    # 3 * G4
      -0.447213595499958    # -1 + 4 * G4
    ]

    # (sqrt(5) - 1)/4 = F4, used once below
    F4 = T(0.309016994374947451)

  # First corner
  var i  = floor(v + dot(v, [F4].xxxx))
  let x0 = v -   i + dot(i, C.xxxx)

  # Other corners

  # Rank sorting originally contributed by Bill Licea-Kane, AMD (formerly ATI)
  let isX = step(x0.yzw, x0.xxx)
  let isYZ = step(x0.zww, x0.yyz)
  var i0 = vec(isX.x + isX.y + isX.z, T(1) - isX)
  i0.y += isYZ.x + isYZ.y
  i0.z += T(1) - isYZ.x
  i0.w += T(1) - isYZ.y
  i0.z += isYZ.z
  i0.w += T(1) - isYZ.z

  # i0 now contains the unique values 0,1,2,3 in each channel
  let i3 = clamp(i0, T(0), T(1))
  let i2 = clamp(i0 - T(1), T(0), T(1))
  let i1 = clamp(i0 - T(2), T(0), T(1))

  let x1 = x0 - i1 + C.x
  let x2 = x0 - i2 + C.y
  let x3 = x0 - i3 + C.z
  let x4 = x0 + C.w

  # Permutations
  i = floorMod(i, [T(289)].xxxx)
  let j0: T = permute(permute(permute(permute(i.w) + i.z) + i.y) + i.x)
  let
    j1w = [i1.w, i2.w, i3.w, 1] <$> permute a + i.w
    j1z = ([i1.z, i2.z, i3.z, 1],j1w) <$> permute a + b + i.z
    j1y = ([i1.y, i2.y, i3.y, 1],j1z) <$> permute a + b + i.y
    j1 = ([i1.x, i2.x, i3.x, 1],j1y) <$> permute a + b + i.x

  # Gradients: 7x7x6 points over a cube, mapped onto a 4-cross polytope
  # 7*7*6 = 294, which is close to the ring size 17*17 = 289.
  let ip = [T(294),49,7,0] <$> T(1)/a

  var p0 = gtc_grad4(j0,   ip)
  var p1 = gtc_grad4(j1.x, ip)
  var p2 = gtc_grad4(j1.y, ip)
  var p3 = gtc_grad4(j1.z, ip)
  var p4 = gtc_grad4(j1.w, ip)

  # Normalise gradients
  let norm = [p0,p1,p2,p3] <$> a.norm2.taylorInvSqrt
  p0 *= norm.x
  p1 *= norm.y
  p2 *= norm.z
  p3 *= norm.w
  p4 *= detail.taylorInvSqrt(dot(p4, p4))

  # Mix contributions from the five corners
  var m0 = max(T(0.6) - [x0,x1,x2].fmap(a.norm2), [T(0)].xxx)
  var m1 = max(T(0.6) - [x3,x4].fmap(a.norm2), [T(0)].xx)
  m0 = m0 * m0
  m1 = m1 * m1
  return T(49) *
    (dot(m0 * m0, [dot(p0, x0), dot(p1, x1), dot(p2, x2)]) +
    dot(m1 * m1, [dot(p3, x3), dot(p4, x4)]))


when isMainModule:
  # for performance analysis with `perf`
  {.passC: "-fno-omit-frame-pointer".}
  var line = newStringOfCap(80)

  import typetraits

  proc drawNoise[T : SomeFloat](applyNoise: proc(x,y: T): T): void =
    var h: T = -Inf
    var l: T = Inf

    for y in 0 ..< 40:
      for x in 0 ..< 80:
        let n = applyNoise(T(x) * T(0.1), T(y) * T(0.1))
        h = max(h, n)
        l = min(l, n)

        # clamp is only needed, because `simplex(Vec3d)` is
        # broken. `simplex(Vec3f)` works flawlessly.
        let i = int(clamp((n + 1) * 5,0,9))
        line.add "  .:+=*%#@"[i]
      echo line
      line.setLen(0)

    echo "min: ", l, " max: ", h

  proc drawNoise[T](typ: typedesc[T]): void =
    echo "testing type: ", typ.name

    drawNoise do (x,y: T) -> T:
      perlin [x, y]

    drawNoise do (x,y: T) -> T:
      perlin [x, y, (x*y) * T(0.5)]

    drawNoise do (x,y: T) -> T:
      perlin [x, y, (x*y) * T(0.5), (x + y)]

    drawNoise do (x,y: T) -> T:
      perlin [x, y], [T(4)].xx

    drawNoise do (x,y: T) -> T:
      perlin [x, y, 0.5], [T(4)].xxx

    drawNoise do (x,y: T) -> T:
      perlin [x, y, 0.5, 0.5], [T(4)].xxxx

    drawNoise do (x,y: T) -> T:
      simplex [x, y]

    drawNoise do (x,y: T) -> T:
      simplex [x, y, (x*y) * T(0.5)]

    drawNoise do (x,y: T) -> T:
      simplex [x, y, (x*y) * T(0.5), (x + y)]

  drawNoise(float32)
  drawNoise(float64)


  import times, random
  proc benchmark() =
    echo "benchmark"

    var accu: float32 # accumulate garbage

    var samplePos: array[4, float32]

    let t = getTime()

    for i in 0 ..< 10_000_000:
      samplePos = [
        float32(rand(100.0f)),
        rand(100.0f),
        rand(100.0f),
        rand(100.0f)
      ]
      accu += simplex[float32](samplePos)

    let duration = getTime() - t

    echo accu # the content is garbage, but when not printed it's calculation might get eleminated.
    echo "time: ", duration
    # time: 5 seconds, 596 milliseconds, 10 microseconds, and 646 nanoseconds
    # non release: 2 minutes, 15 seconds, 732 milliseconds, 415 microseconds, and 820 nanoseconds
    #     release: 3 seconds, 915 milliseconds, 559 microseconds, and 718 nanoseconds

  benchmark()
