type vec3d = { x:float; y:float; z:float }

let splat3 u = { x=u; y=u; z=u }
let ( *| ) a b = { x=a.x*.b.x; y=a.y*.b.y; z=a.z*.b.z }
let ( +| ) a b = { x=a.x+.b.x; y=a.y+.b.y; z=a.z+.b.z }
let ( -| ) a b = { x=a.x-.b.x; y=a.y-.b.y; z=a.z-.b.z }
let ( /| ) a b = { x=a.x/.b.x; y=a.y/.b.y; z=a.z/.b.z }
let dot3 a b = a.x*.b.x+.a.y*.b.y+.a.z*.b.z
let length3 v = sqrt (dot3 v v)
let cross3 a b = { x=a.y*.b.z-.a.z*.b.y; y=a.z*.b.x-.a.x*.b.z;z=a.x*.b.y-.a.y*.b.x }
let normalize a = let n = 1.0 /. length3 a in a *| splat3 n

type camera = {
  org:vec3d;
  up:vec3d;
  view:vec3d;
  imgplaneorg:vec3d; xaxis:vec3d; zaxis:vec3d;
  fov:float; ratio:float; dist:float
}
let pi = 3.1415926535897932384626433832795
let huge = 1e9
let makecamera org up view fov ratio =
  let left = splat3 (ratio *. -0.5) in
  let top = splat3 0.5 in
  let dist = splat3 (0.5 /. tan (fov *. pi /. 360.0)) in
  let nview = normalize view in
  let nup = normalize up in
  let nxaxis = cross3 nview nup in
  let nzaxis = cross3 nview nxaxis in
  let imgplaneorg = dist*|nview +| left*|nxaxis -| top*|nzaxis in
  let scaledxaxis = nxaxis *| splat3 ratio in
  {org=org; up=nup; view=nview;
   imgplaneorg=imgplaneorg;
   xaxis=scaledxaxis; zaxis=nzaxis;
   fov=fov; ratio=ratio; dist=dist.x}

type ray = {
  org:vec3d; dir:vec3d;
  tnear:float; tfar:float
}

type sphere = { org:vec3d; radius:float }

let sphere_getnormal s v = (v-|s.org) /| splat3 s.radius
let sphere_intersect (s:sphere) (r:ray) =
  let v = s.org -| r.org in
  let b = dot3 r.dir v in
  let disc = b*.b-.dot3 v v +. s.radius*.s.radius in
  if disc < 0.0 then
    huge
  else begin
    let d = sqrt disc in
    let t1 = b-.d in
    let t2 = b+.d in
    if t2 < 0.0 then huge else if t1 > 0.0 then t1 else t2
  end

let generateray cam w h x y =
  let rw = splat3 (1.0 /. float w) in
  let rh = splat3 (1.0 /. float h) in
  let fx = splat3 (float x) in
  let fy = splat3 (float y) in
  let nxaxis = cam.xaxis *| rw in
  let nzaxis = cam.zaxis *| rh in
  let dir = normalize (cam.imgplaneorg +| fx*|nxaxis +| fy*|nzaxis) in
  {org=cam.org; dir=dir; tnear=0.0; tfar=huge}

(* rendering parameters *)
let w = 1024
let h = 1024
let ldir = normalize {x=1.0;y=1.0;z=1.0}
let cam = makecamera {x=5.0;y=(-10.0);z=10.0} {x=0.0;y=0.0;z=1.0} {x=0.0;y=1.0;z=0.0} 45.0 1.0

(* create a 3d array of spheres *)
let spheres = Array.make (16*16*16) {org = {x=0.0; y=0.0; z=0.0}; radius=0.5};;
for x = 0 to 15 do
  for y = 0 to 15 do
    for z = 0 to 15 do
      spheres.(x*256+y*16+z) <- {org = {x=float x; y=float y; z=float z}; radius=0.5}
    done
  done
done;

(* image computation *)
Printf.printf "P2\n%d %d\n256\n" w h
let closest = ref huge
let closestnormal = ref (splat3 0.0)
let isec (r:ray) (s:sphere) =
  let d = sphere_intersect s r in
  if d < !closest then begin
    closest := d;
    closestnormal := sphere_getnormal s (r.org +| r.dir *| splat3 d);
  end;;

for y = 0 to h-1 do
  for x = 0 to w-1 do
    let r = generateray cam w h x y in
    closest := huge;
    closestnormal := splat3 0.0;
    Array.iter (isec r) spheres;
    if !closest = huge then
      Printf.printf "0 "
    else
      let l = min (max (abs_float (dot3 (normalize !closestnormal) ldir)) 0.0) 1.0 in
      let d = int_of_float (255.0 *. l) in
      Printf.printf "%d " d
  done
done


