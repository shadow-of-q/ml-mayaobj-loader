open Mayaobj
open Raytracer

let debug = true
let out = if debug then stderr else open_out "/dev/null"

let scene_name = "data/teapot.obj";;

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
end;

Raytracer.raytrace scene

