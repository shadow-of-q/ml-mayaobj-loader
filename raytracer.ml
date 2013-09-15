open Mayaobj

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
  cam_org: vec3d;
  cam_up: vec3d;
  cam_view: vec3d;
  cam_imgplaneorg: vec3d;
  cam_xaxis: vec3d;
  cam_zaxis: vec3d;
  cam_fov: float;
  cam_ratio: float;
  cam_dist: float
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
     cam_org = org; cam_up = nup; cam_view = nview;
     cam_imgplaneorg = imgplaneorg;
     cam_xaxis = scaledxaxis; cam_zaxis = nzaxis;
     cam_fov = fov; cam_ratio = ratio; cam_dist = dist.x
  }

type ray = {
  org: vec3d;
  dir: vec3d;
  tnear: float;
  tfar: float
}

let generateray cam w h x y =
  let rw = splat (1.0 /. float w) in
  let rh = splat (1.0 /. float h) in
  let fx = splat (float x) in
  let fy = splat (float y) in
  let nxaxis = cam.cam_xaxis *| rw in
  let nzaxis = cam.cam_zaxis *| rh in
  let dir = normalize (cam.cam_imgplaneorg +| fx *| nxaxis +| fy *| nzaxis) in
  { org = cam.cam_org; dir = dir; tnear = 0.0; tfar = huge }

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
let w = 64
let h = 64
let ldir = normalize { x = 1.0; y = 0.5; z = 0.3 }
let cam = makecamera
  { x = 2.0; y = 25.0; z = -200.0 }
  { x = 0.0; y = 1.0;  z = 0.0 }
  { x = 0.0; y = 0.0;  z = 1.0 }
  45.0 1.0

(* image computation *)
let raytrace scene =
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

