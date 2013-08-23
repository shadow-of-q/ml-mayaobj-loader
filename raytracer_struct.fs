open Microsoft.FSharp.Core
open System

type vec3d =
  struct
    val x: float
    val y: float
    val z: float
    new (x_:float, y_:float, z_:float) = {x=x_; y=y_; z=z_}
  end

// type vec3d = { x:float; y:float; z:float }

let inline splat3 u = new vec3d(u,u,u)
let inline ( *| ) (a:vec3d) (b:vec3d) = new vec3d(a.x*b.x, a.y*b.y, a.z*b.z)
let inline ( +| ) (a:vec3d) (b:vec3d) = new vec3d(a.x+b.x, a.y+b.y, a.z+b.z)
let inline ( -| ) (a:vec3d) (b:vec3d) = new vec3d(a.x-b.x, a.y-b.y, a.z-b.z)
let inline ( /| ) (a:vec3d) (b:vec3d) = new vec3d(a.x/b.x, a.y/b.y, a.z/b.z)
let inline dot3 (a:vec3d) (b:vec3d) = a.x*b.x+a.y*b.y+a.z*b.z
let inline length3 (v:vec3d) = Math.Sqrt (dot3 v v)
let inline cross3 (a:vec3d) (b:vec3d) = new vec3d(a.y*b.z-a.z*b.y, a.z*b.x-a.x*b.z, a.x*b.y-a.y*b.x)
let inline normalize a:vec3d =
  let n = 1.0 / length3 a
  a *| splat3 n

type camera = {
  org:vec3d;
  up:vec3d;
  view:vec3d;
  imgplaneorg:vec3d; xaxis:vec3d; zaxis:vec3d;
  fov:float; ratio:float; dist:float
}
let makecamera org up view fov ratio =
  let left = splat3 (ratio * -0.5)
  let top = splat3 0.5
  let dist = splat3 (0.5 / Math.Tan (fov * Math.PI / 360.0))
  let nview = normalize view
  let nup = normalize up
  let nxaxis = cross3 nview nup
  let nzaxis = cross3 nview nxaxis
  let imgplaneorg = dist*|nview +| left*|nxaxis -| top*|nzaxis
  let scaledxaxis = nxaxis *| splat3 ratio
  {org=org; up=nup; view=nview;
   imgplaneorg=imgplaneorg;
   xaxis=scaledxaxis; zaxis=nzaxis;
   fov=fov; ratio=ratio; dist=dist.x}

type ray = {
  org:vec3d; dir:vec3d;
  tnear:float; tfar:float
}

type sphere = { org:vec3d; radius:float }
let huge = 1e9

let inline sphere_getnormal s v = (v-|s.org) /| splat3 s.radius
let inline sphere_intersect (s:sphere) (r:ray) = 
  let v = s.org -| r.org
  let b = dot3 r.dir v
  let disc = b*b-dot3 v v  + s.radius*s.radius
  if disc < 0.0 then
    huge
  else
    let d = Math.Sqrt disc
    let t1 = b-d
    let t2 = b+d
    if t2 < 0.0 then huge else if t1 > 0.0 then t1 else t2

let generateray cam w h x y =
  let rw = splat3 (1.0 / float w)
  let rh = splat3 (1.0 / float h)
  let fx = splat3 (float x)
  let fy = splat3 (float y)
  let nxaxis = cam.xaxis *| rw
  let nzaxis = cam.zaxis *| rh
  let dir = normalize (cam.imgplaneorg +| fx*|nxaxis +| fy*|nzaxis)
  {org=cam.org; dir=dir; tnear=0.0; tfar=huge}

(* rendering parameters *)
let w = 1024
let h = 1024
let ldir = normalize (new vec3d(1.0, 1.0, 1.0))
let cam = makecamera (new vec3d(5.0,-10.0,10.0)) (new vec3d(0.0,0.0,1.0)) (new vec3d(0.0,1.0,0.0)) 45.0 1.0

(* create a 3d array of spheres *)
let grid = new vec3d(16.0, 16.0, 16.0)
let spheres = [|
  for x in 0 .. int grid.x do
    for y in 0 .. int grid.y do
      for z in 0 .. int grid.z do
        yield {org = new vec3d(float x, float y, float z); radius=0.5}|]

(* image computation *)
Console.Write("P2\n{0} {1}\n256\n", w, h)
for y in 0 .. h-1 do
  for x in 0 .. w-1 do
    let r = generateray cam w h x y
    let mutable closest = huge
    let mutable closestnormal = splat3 0.0
    for s in spheres do
      let d = sphere_intersect s r
      if d < closest then
        closest <- d
        closestnormal <- sphere_getnormal s (r.org +| r.dir *| splat3 d)
    if closest = huge then
      Console.Write("{0} ", 0)
    else
      let p = r.org +| r.dir *| splat3 closest
      let l = min (max (abs (dot3 (normalize closestnormal) ldir)) 0.0) 1.0
      Console.Write("{0} ", int(255.0*l))
  printf "\n"

