open Mayaobj;;

(* to compare indexed vertices *)
module IndexedVertex =
  struct
    type t = int array
    let compare v0 v1 =
           if v0.(0) != v1.(0) then (if v0.(0) < v1.(0) then 1 else -1)
      else if v0.(1) != v1.(1) then (if v0.(1) < v1.(1) then 1 else -1)
      else if v0.(2) != v1.(2) then (if v0.(2) < v1.(2) then 1 else -1)
      else 0
  end

(* immutable map of indexing vertices *)
module Vertexmap = Map.Make(IndexedVertex)

let default_pos = {x = 0.0; y = 0.0; z = 0.0}
let default_nor = {x = 0.0; y = 0.0; z = 1.0}
let default_tex = {x = 0.0; y = 0.0; z = 0.0}
let default_vert = {p = default_pos; n = default_nor; t = default_tex}
let default_material = {
  mat_name = "#default";
  mat_mapka = "";
  mat_mapkd = "";
  mat_mapd = "";
  mat_mapbump = "";
  mat_km = 0.0;
  mat_reflect = 0.0;
  mat_refract = 0.0;
  mat_trans = 0.0;
  mat_shiny = 0.0;
  mat_glossy = 0.0;
  mat_refract_index = 0.0;
  mat_illum = 0;
  mat_amb = {x = 0.0; y = 0.0; z = 0.0};
  mat_diff = {x = 0.0; y = 0.0; z = 0.0};
  mat_spec = {x = 0.0; y = 0.0; z = 0.0}
}

(* helper / syntactic sugars *)
let fos = float_of_string and ios = int_of_string
let htadd = Hashtbl.add and htfind = Hashtbl.find
let vadd = Vertexmap.add and vfind = Vertexmap.find
let scan = Scanf.sscanf
let split = Str.split (Str.regexp "[\t\ \r]+")
let rgb r g b = {x = fos r; y = fos b; z = fos g}

(* be careful with negative indices *)
let non_negative p =
  if p.(0) < 0 then raise (Failure "negative indices are not supported");
  if p.(1) < 0 then raise (Failure "negative indices are not supported");
  if p.(2) < 0 then raise (Failure "negative indices are not supported");
  p

(* parse a 3d vector made of x y z *)
let parse_vec3 l = match l with
    x :: [] -> {x = fos x; y = 0.0; z = 0.0}
  | x :: y :: [] -> {x = fos x; y = fos y; z = 0.0}
  | x :: y :: z :: [] -> {x = fos x; y = fos y; z = fos z}
  | _ -> raise (Failure "improperly formed 3d vector")

(* encapsulate exception into an option *)
let read_line chan = try Some (input_line chan) with
  End_of_file -> close_in chan; None

(* make an array from a *reversed* list *)
let makearray l default =
  let len = List.length l in
  if len = 0 then [||]
  else
    let arr = Array.make len default in
      List.iteri (fun index x -> arr.(len-index-1) <- x) l;
    arr

(* append a new entry in the diagnostic summary *)
let diag_add diag file no str = (file, no, str) :: diag
let diag_unknown_token diag file no tok = diag_add diag file no ("unknown token " ^ tok)

(* parse the wavefront .mtl file in a functional coding style *)
let loadmtl file diag =

  (* open the file and recursively build all matlist *)
  let chan = open_in file in

  (* recursively parse the file line by line *)
  let rec doline all mat diag no =
    let next all mat = doline all mat diag (no+1) in
    match read_line chan with
      None -> (mat :: all, mat, diag)
    | Some line -> match split line with
        [] -> next all mat
      | "newmtl" :: str :: [] -> next (mat :: all) {default_material with mat_name = str}
      | "Ka" :: r :: g :: b :: [] -> next all {mat with mat_amb  = rgb r g b}
      | "Kd" :: r :: g :: b :: [] -> next all {mat with mat_diff = rgb r g b}
      | "Ks" :: r :: g :: b :: [] -> next all {mat with mat_spec = rgb r g b}
      | "Ns" :: x :: [] -> next all {mat with mat_shiny = fos x}
      | "Km" :: x :: [] -> next all {mat with mat_km = fos x}
      | "d" :: x :: [] -> next all {mat with mat_trans = fos x}
      | "r" :: x :: [] -> next all {mat with mat_reflect = fos x}
      | "sharpness" :: x :: [] -> next all {mat with mat_glossy = fos x}
      | "Ni" :: x :: [] -> next all {mat with mat_refract_index = fos x}
      | "map_Ka" :: str :: [] -> next all {mat with mat_mapka = str}
      | "map_Kd" :: str :: [] -> next all {mat with mat_mapkd = str}
      | "map_D" :: str :: [] -> next all {mat with mat_mapd = str}
      | "map_Bump" :: str :: [] -> next all {mat with mat_mapbump = str}
      | "illum" :: x :: [] -> next all {mat with mat_illum = ios x}
      | tok :: rest -> doline all mat (diag_unknown_token diag file no tok) (no+1) in

  (* build material lists and properly store everything in arrays and tables *)
  match doline [] default_material diag 1 with
    matlist, _, diag ->
      let matarray = makearray matlist default_material in
      let matmap = Hashtbl.create (Array.length matarray) in
      Array.iteri (fun index m -> htadd matmap m.mat_name (index)) matarray;
      (matarray, matmap, diag)
;;

(* store the complete state of the parser *)
type state = {
  file: string;
  pos: vec3d list;
  nor: vec3d list;
  tex: vec3d list;
  tris: triangle list;
  matarray: material array;
  matmap: (string, int) Hashtbl.t;
  vertmap: int Vertexmap.t;
  vertnum: int;
  currmat: int;
  lineno: int;
  diag: (string * int * string) list; (* line number, filename, message *)
}

let default_state = {
  file = "";
  pos = [default_pos];
  nor = [default_nor];
  tex = [default_tex];
  tris = [];
  matarray = [||];
  matmap = Hashtbl.create 0;
  vertmap = Vertexmap.empty;
  vertnum = 0;
  currmat = 0;
  lineno = 1;
  diag = []
}

(* various state updates *)
let st_next st = { st with lineno = st.lineno + 1 }
let st_failure st str = { st with diag = diag_add st.diag st.file st.lineno str }
let st_failure_next st str = { (st_failure st str) with lineno = st.lineno + 1 }

(* update position array *)
let st_new_pos st tokens =
  try let v = parse_vec3 tokens in { (st_next st) with pos = v :: st.pos }
  with Failure str -> st_failure_next st str

(* update normal array *)
let st_new_nor st tokens =
  try let v = parse_vec3 tokens in { (st_next st) with nor = v :: st.nor }
  with Failure str -> st_failure_next st str

let match_vert_pn_re = Str.regexp "[0-9]+//[0-9]+"
let split_vert_pn_re = Str.regexp "//"
let split_vert_re = Str.regexp "/"
let improper_vertex_index = Failure "improperly formed vertex index"

(* append a new vertex in the vertex map. much slower than C strstr version *)
let st_new_vertex st str =
  let v =
    if Str.string_match match_vert_pn_re str 0 then
      match List.map ios (Str.split split_vert_pn_re str) with
        p :: n :: [] -> [|p; 0; n|]
      | _ -> raise improper_vertex_index
    else
      match List.map ios (Str.split split_vert_re str) with
        p :: [] -> [|p; 0; 0|]
      | p :: t :: [] -> [|p; t; 0|]
      | p :: t :: n :: [] -> [|p; t; n|]
      | _ -> raise improper_vertex_index in
    try
      (st, vfind (non_negative v) st.vertmap)
    with Not_found ->
      let map = vadd v st.vertnum st.vertmap in
      ({st with vertmap = map; vertnum = st.vertnum + 1}, st.vertnum)

(* update texture coordinate array *)
let st_new_tex st tokens =
  try let v = parse_vec3 tokens in { (st_next st) with tex = v :: st.tex }
  with Failure str -> st_failure_next st str

(* append a new face *)
let st_new_face st tokens =

  (* update the state with a new face *)
  let append oldstate v0 v1 v2 = { oldstate with tris = {
      tri_vert = {xi = v0; yi = v1; zi = v2};
      tri_mat = oldstate.currmat
    } :: oldstate.tris
  } in

  (* pair = (state, [vertex IDs]) *)
  let foldvertex pair vstr = match pair with
    oldstate, lst -> match st_new_vertex oldstate vstr with
      newstate, n -> (newstate, n :: lst) in

  (* compute the list of indices and get the updated state from it *)
  match List.fold_left foldvertex (st, []) tokens with
    lateststate, indices -> match indices with
      v0 :: v1 :: v2 :: [] -> st_next (append lateststate v0 v1 v2)
    | v0 :: v1 :: v2 :: v3 :: [] -> st_next (append (append lateststate v0 v1 v2) v0 v2 v3)
    | _ -> st_failure_next lateststate "only triangles and quads are suported for now"

(* load the mtl file and return an updated state *)
let st_new_mtl st file =
  match loadmtl file st.diag with
    matarray, matmap, diag -> { (st_next st)
      with matarray = matarray; matmap = matmap; diag = diag
    }

(* change the current material *)
let st_new_curr_mat st name =
  let currmat = (try htfind st.matmap name with Not_found -> 0) in
  { (st_next st) with currmat = currmat }

(* parse the wavefront .obj file in a functional coding style *)
let loadobj filename =

  (* open the file and recursively build all matlist *)
  let chan = open_in filename in

  (* recursively parse the file line by line *)
  let rec doline st =
    match read_line chan with
      None -> st
    | Some line -> match split line with
        "v"  :: rest -> doline (st_new_pos st rest)
      | "vn" :: rest -> doline (st_new_nor st rest)
      | "vt" :: rest -> doline (st_new_tex st rest)
      | "f"  :: rest -> doline (st_new_face st rest)
      | "mtllib"  :: name :: [] -> doline (st_new_mtl st name)
      | "usemtl" :: name :: [] -> doline (st_new_curr_mat st name)
      | _ -> doline (st_next st) in

  (* parse the complete obj file *)
  let st = doline default_state in

  (* triangle array is simply built from its list *)
  let triarray = makearray st.tris {tri_mat = 0; tri_vert = {xi = 0; yi = 0; zi = 0}} in

  (* vertex arrays are made from the vertex indexed list and individual data arrays *)
  let posarray = makearray st.pos default_pos in
  let norarray = makearray st.nor default_nor in
  let texarray = makearray st.tex default_tex in
  let vertnum = Vertexmap.cardinal st.vertmap in
  let vertarray = Array.make vertnum default_vert in
    Vertexmap.iter (fun key value -> vertarray.(value) <- {
      p = posarray.(key.(0));
      t = texarray.(key.(1));
      n = norarray.(key.(2));
    }) st.vertmap;
  { triangles = triarray; vertices = vertarray; materials = st.matarray }

