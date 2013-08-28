open Mayaobj;;

let default_pos = {x = 0.0; y = 0.0; z = 0.0}
let default_nor = {x = 0.0; y = 0.0; z = 1.0}
let default_tex = {x = 0.0; y = 0.0; z = 0.0}
let default_vert = {p = default_pos; n = default_nor; t = default_tex}
let default_material = {
  name = "#default";
  mapka = "";
  mapkd = "";
  mapd = "";
  mapbump = "";
  km = 0.0;
  reflect = 0.0;
  refract = 0.0;
  trans = 0.0;
  shiny = 0.0;
  glossy = 0.0;
  refract_index = 0.0;
  illum = 0;
  amb = {x = 0.0; y = 0.0; z = 0.0};
  diff = {x = 0.0; y = 0.0; z = 0.0};
  spec = {x = 0.0; y = 0.0; z = 0.0};
}

(* encapsulate exception into an option *)
let read_line chan = try Some (input_line chan) with
  End_of_file -> close_in chan; None

let fos = float_of_string

(* parse the wavefront .mtl file *)
let loadmtl filename =

  (* all materials found in the obj file *)
  let materials = ref [] in
  let append m = materials := m :: !materials in

  (* define the process done line by line for a .mtl file *)
  let do_mtl_line mat line =
    let tokens = Str.split (Str.regexp "[\t\ \r]+") line in
    match tokens with
      (* append the previous material and start a new one *)
      | "newmtl" :: name :: [] -> append mat; {default_material with name=name}
      | "Ka" :: r :: g :: b :: [] -> {mat with amb  = {x = fos r; y = fos b; z = fos g}}
      | "Kd" :: r :: g :: b :: [] -> {mat with diff = {x = fos r; y = fos b; z = fos g}}
      | "Ks" :: r :: g :: b :: [] -> {mat with spec = {x = fos r; y = fos b; z = fos g}}
      | "Ns" :: x :: [] -> {mat with shiny = fos x}
      | "Km" :: x :: [] -> {mat with km = fos x}
      | "d" :: x :: [] -> {mat with trans = fos x}
      | "r" :: x :: [] -> {mat with reflect = fos x}
      | "sharpness" :: x :: [] -> {mat with glossy = fos x}
      | "Ni" :: x :: [] -> {mat with refract_index = fos x}
      | "map_Ka" :: str :: [] -> {mat with mapka = str}
      | "map_Kd" :: str :: [] -> {mat with mapkd = str}
      | "map_D" :: str :: [] -> {mat with mapd = str}
      | "map_Bump" :: str :: [] -> {mat with mapbump = str}
      | "illum" :: x :: [] -> {mat with illum = int_of_string x}
      | _ -> mat in

  (* open the file and recursively build all materials *)
  let chan = open_in filename in
  let rec process_mtl chan mat =
    let line = read_line chan in match line with
      | Some x -> process_mtl chan (do_mtl_line mat x)
      | None -> mat in

  (* first material will be default one *)
  append (process_mtl chan default_material);
  !materials

(* make an array from a *reversed* list *)
let makearray l default =
  let len = List.length l in
  if len = 0 then [||]
  else
    let arr = Array.make len default in
      List.iteri (fun index x -> arr.(len-index-1) <- x) l;
    arr

(* parse the wavefront obj file *)
let loadobj filename =

  let htadd = Hashtbl.add and htfind = Hashtbl.find in
  let tris = ref [] in
  let vertices = Hashtbl.create 16 in (* contains indexed .obj vertices *)
  let pos = ref [default_pos] in
  let nor = ref [default_nor] in
  let tex = ref [default_tex] in
  let currmat = ref 0 in (* current material *)
  let vertn = ref 0 in (* number of generated vertices *)
  let materials = ref [] in

  (* define the process done line by line for an .obj file *)
  let do_obj_line line =
    let tokens = Str.split (Str.regexp "[\t\ \r]+") line in

    (* parse a 3d vector made of x y z *)
    let do_vec3 m l = match l with
      | x :: y :: z :: [] -> m := {x = fos x; y = fos y; z = fos z} :: !m
      | _ -> raise (Failure "improperly formed 3d vector")
    in

    (* a vertex is p or p/t or p/t/n or p//n *)
    let do_vertex str =
      let scan = Scanf.sscanf in
      let v = try (* really slow that way but it is simple *)
        scan str "%d/%d/%d" (fun p t n -> [|p; t; n|]) with _ -> try
        scan str "%d/%d" (fun p t -> [|p; t; 0|]) with _ -> try
        scan str "%d//%d" (fun p n -> [|p; 0; n|]) with _ ->
        scan str "%d" (fun p -> [|p; 0; 0|]) in
      try htfind vertices v with Not_found -> htadd vertices v !vertn;
      incr vertn;
      !vertn - 1
    in

    (* a face is made of variable number of vertices *)
    let do_face f =
      let append v0 v1 v2 =
        tris := {
          v = {xi = do_vertex v0; yi = do_vertex v1; zi = do_vertex v2};
          m = !currmat
        } :: !tris in
      match f with
        | v0 :: v1 :: v2 :: [] -> append v0 v1 v2
        | v0 :: v1 :: v2 :: v3 :: [] -> append v0 v1 v2; append v0 v2 v3
        | _ -> raise (Failure "only triangles and quads are suported for now") in

    (* parse a line of text in the obj file *)
    let do_line l = match l with
      | "v" :: rest -> do_vec3 pos rest
      | "vn" :: rest -> do_vec3 nor rest
      | "vt" :: rest -> do_vec3 tex rest
      | "f" :: rest -> do_face rest
      | "mtllib" :: filename :: [] -> materials := loadmtl filename
      | _ -> () (* ignore the rest, including comments and groups *)
    in do_line tokens
  in
  (* open the file and parse it line by line *)
  let chan = open_in filename in
  let rec process_obj chan =
    let line = read_line chan in match line with
      | Some x -> do_obj_line x; process_obj chan
      | None -> () in
  process_obj chan;
  materials := if !materials = [] then [default_material] else !materials;

  (* triangle and material arrays are simply built from their respective lists *)
  let tri_array = makearray !tris {m = 0; v = {xi = 0; yi = 0; zi = 0}} in
  let mat_array = makearray !materials default_material in

  (* vertex arrays are made from the vertex indexed list and individual data arrays *)
  let pos_array = makearray !pos default_pos in
  let nor_array = makearray !nor default_nor in
  let tex_array = makearray !tex default_tex in
  let vert_array = Array.make (Hashtbl.length vertices) default_vert in
    Hashtbl.iter (fun key value -> vert_array.(value) <- {
      p = pos_array.(key.(0));
      t = tex_array.(key.(1));
      n = nor_array.(key.(2));
    }) vertices;
  {triangles = tri_array; vertices = vert_array; materials = mat_array};;

