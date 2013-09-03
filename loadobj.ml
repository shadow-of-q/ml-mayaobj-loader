open Mayaobj

let debug = true
let out = if debug then stdout else open_out "/dev/null"

let scene_name = "data/conference.obj";;

Printf.fprintf out "starting to load %s\n" scene_name
let start = Sys.time()
let scene = Mayaobjpureparser.loadobj scene_name
let finish = Sys.time();;

if debug then begin
  Printf.fprintf out "%s loaded in %f seconds \n" scene_name (finish -. start);

  (* scene description *)
  Printf.fprintf out "%d vertices\n%d triangles\n%d materials\n"
    (Array.length scene.vertices)
    (Array.length scene.triangles)
    (Array.length scene.materials);
  if false then begin
    (* all vertices *)
    Array.iteri (fun index v ->
      Printf.fprintf out "v -> %d\tp %f %f %f\n" index v.p.x v.p.y v.p.z;
      Printf.fprintf out "\tn %f %f %f\n" v.n.x v.n.y v.n.z;
      Printf.fprintf out "\tt %f %f %f\n" v.t.x v.t.y v.t.z) scene.vertices;

    (* all indexed triangles *)
    Array.iteri (fun index t -> Printf.fprintf out "t -> %d\tm -> %d %d %d; %d\n"
      index t.tri_vert.xi t.tri_vert.yi t.tri_vert.zi t.tri_mat)
      scene.triangles;

    (* all materials *)
    Array.iteri (fun index m -> Printf.fprintf out "m -> %d %s\n" index m.mat_name;
        Printf.fprintf out "\tmapka %s\n" m.mat_mapka;
        Printf.fprintf out "\tmapkd %s\n" m.mat_mapkd;
        Printf.fprintf out "\tmapd %s\n" m.mat_mapd;
        Printf.fprintf out "\tmapd %s\n" m.mat_mapbump)
      scene.materials;
  end;
  raise (Failure "")
end

let pi = 3.1415926535897932384626433832795
let huge = 1e9
let epsilon = 1e-6
let splat u = { x = u; y = u; z = u }
let ( *| ) a b = { x = a.x*.b.x; y = a.y*.b.y; z = a.z*.b.z }
let ( +| ) a b = { x = a.x+.b.x; y = a.y+.b.y; z = a.z+.b.z }
let ( -| ) a b = { x = a.x-.b.x; y = a.y-.b.y; z = a.z-.b.z }
let ( /| ) a b = { x = a.x/.b.x; y = a.y/.b.y; z = a.z/.b.z }
let dot a b = a.x*.b.x+.a.y*.b.y+.a.z*.b.z
let length v = sqrt (dot v v)
let cross a b = {
  x = a.y*.b.z -. a.z*.b.y;
  y = a.z*.b.x -. a.x*.b.z;
  z = a.x*.b.y -. a.y*.b.x
}
let normalize a = let n = 1.0 /. length a in a *| splat n

type camera = {
  org: vec3d;
  up: vec3d;
  view: vec3d;
  imgplaneorg: vec3d;
  xaxis: vec3d;
  zaxis: vec3d;
  fov: float;
  ratio: float;
  dist: float
}

type ray = {
  org: vec3d;
  dir: vec3d;
  tnear: float;
  tfar: float
}

let makecamera org up view fov ratio =
  let left = splat (ratio *. -0.5) in
  let top = splat 0.5 in
  let dist = splat (0.5 /. tan (fov *. pi /. 360.0)) in
  let nview = normalize view in
  let nup = normalize up in
  let nxaxis = cross nview nup in
  let nzaxis = cross nview nxaxis in
  let imgplaneorg = dist*|nview +| left*|nxaxis -| top*|nzaxis in
  let scaledxaxis = nxaxis *| splat ratio in {
     org = org; up = nup; view = nview;
     imgplaneorg = imgplaneorg;
     xaxis = scaledxaxis; zaxis = nzaxis;
     fov = fov; ratio = ratio; dist = dist.x
  }

let generateray cam w h x y =
  let rw = splat (1.0 /. float w) in
  let rh = splat (1.0 /. float h) in
  let fx = splat (float x) in
  let fy = splat (float y) in
  let nxaxis = cam.xaxis *| rw in
  let nzaxis = cam.zaxis *| rh in
  let dir = normalize (cam.imgplaneorg +| fx *| nxaxis +| fy *| nzaxis) in
  { org = cam.org; dir = dir; tnear = 0.0; tfar = huge }

(* Moeller-trumbore intersection routine *)
let no_isec = (0, [|0.0; 0.0; 0.0|]) (* (isec?, [|u; v; t|] *)
let triangle_isec r t vert =
  let v0 = vert.(t.tri_vert.xi).p in
  let v1 = vert.(t.tri_vert.yi).p in
  let v2 = vert.(t.tri_vert.zi).p in (* 3 vertices *)
  let edge1 = v1 -| v0 and edge2 = v2 -| v0 in (* 2 edges sharing v0 *)
  let pvec = cross r.dir edge2 in (* begin determinant computation *)
  let det = dot edge1 pvec in
  if abs_float det < epsilon then no_isec
  else
    let inv_det = 1.0 /. det in
    let tvec = r.org -| v0 in (* distance from v0 to ray origin *)
    let u = (dot tvec pvec) *. inv_det in
    if u < 0.0 || u > 1.0 then no_isec
    else
      let qvec = cross tvec edge1 in (* prepare v computation *)
      let v = (dot r.dir qvec) *. inv_det in
      if v < 0.0 || u +. v > 1.0 then no_isec
      else
        let t = (dot edge2 qvec) *. inv_det in
        (1, [|u; v; t|])

(* geometric normal for the given triangle (should be precomputed) *)
let triangle_geom_normal t vert = 
  let v0 = vert.(t.tri_vert.xi).p in
  let v1 = vert.(t.tri_vert.yi).p in
  let v2 = vert.(t.tri_vert.zi).p in
  let edge1 = v1 -| v0 and edge2 = v2 -| v0 in
  normalize (cross edge1 edge2)

(* brute force linear intersection of the complete obj mesh *)
let mesh_isec o r =
  let closest = ref huge in
  let closestindex = ref (-1) in
  let closestnormal = ref { x = 1.0; y = 0.0; z = 0.0 } in (* just for flat shading *)
  Array.iteri (fun index t -> match triangle_isec r t o.vertices with
    | (1, uvt) -> if uvt.(2) < !closest then begin
        closest := uvt.(2);
        closestindex := index;
        closestnormal := triangle_geom_normal t o.vertices
      end
    | _ -> ()) o.triangles;
   ((if !closest = huge then 0 else 1), !closestnormal)

(* rendering parameters *)
let w = 1024
let h = 1024
let ldir = normalize { x = 1.0; y = 0.5; z = 0.3 }
let cam = makecamera
  { x = 2.0; y = 25.0; z = -200.0 }
  { x = 0.0; y = 1.0;  z = 0.0 }
  { x = 0.0; y = 0.0;  z = 1.0 }
  45.0 1.0
;;

(* image computation *)
Printf.printf "P2\n%d %d\n256\n" w h;
for y = 0 to h-1 do
  for x = 0 to w-1 do
    let r = generateray cam w h x y in
    match mesh_isec scene r with
      | 0, _ -> Printf.printf "0 "
      | _, n ->
        let l = min (max (abs_float (dot n ldir)) 0.0) 1.0 in
        let d = int_of_float (255.0 *. l) in
        Printf.printf "%d " d
  done;
  Printf.printf "\n"
done;;

