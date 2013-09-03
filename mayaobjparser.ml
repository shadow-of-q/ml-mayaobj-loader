open Mayaobj

let default_pos = { x = 0.0; y = 0.0; z = 0.0 }
let default_nor = { x = 0.0; y = 0.0; z = 1.0 }
let default_tex = { x = 0.0; y = 0.0; z = 0.0 }
let default_vert = { p = default_pos; n = default_nor; t = default_tex }
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
  mat_amb  = { x = 0.0; y = 0.0; z = 0.0 };
  mat_diff = { x = 0.0; y = 0.0; z = 0.0 };
  mat_spec = { x = 0.0; y = 0.0; z = 0.0 }
}

(* warnings / errors during parsing *)
exception ParsingError of string
exception ParsingWarning of string

(* helper / syntactic sugars *)
let fos x = try float_of_string x with Failure str -> raise (ParsingError str)
let ios x = try int_of_string x with Failure str -> raise (ParsingError str)
let soi = string_of_int
let htadd = Hashtbl.add and htfind = Hashtbl.find
let scan = Scanf.sscanf
let split = Str.split (Str.regexp "[\t\ \r]+")
let rgb r g b = { x = fos r; y = fos b; z = fos g }

(* append a new diagnostic *)
let append_error_warning lst filename str lineno =
  let entry = "file '" ^ filename ^ "': line " ^ (soi lineno) ^ ": " ^ str in
  lst := entry :: !lst

(* print out all the diagnostics *)
let report_error_warning str lst =
  Printf.fprintf stderr "%d %s\n" (List.length lst) str;
  List.iter (Printf.fprintf stderr "%s\n") (List.rev lst)

(* encapsulate exception into an option *)
let read_line chan = try Some (input_line chan) with
  End_of_file -> close_in chan; None

(* be careful with negative indices *)
let non_negative p =
  if p.(0) < 0 then raise (ParsingError "negative indices are not supported");
  if p.(1) < 0 then raise (ParsingError "negative indices are not supported");
  if p.(2) < 0 then raise (ParsingError "negative indices are not supported");
  p

(* parse the wavefront .mtl file *)
let loadmtl filename =

  (* all matlist found in the obj file *)
  let matlist = ref [] in
  let append m = matlist := m :: !matlist in

  (* define the process done line by line for a .mtl file *)
  let do_mtl_line mat line lineno =
    let trimmed = String.trim line in
    if String.length trimmed > 0 && String.get trimmed 0 != '#' then
      match split trimmed with
        "newmtl" :: name :: [] -> append mat; {default_material with mat_name = name}
      | "Ka" :: r :: g :: b :: [] -> {mat with mat_amb  = rgb r g b}
      | "Kd" :: r :: g :: b :: [] -> {mat with mat_diff = rgb r g b}
      | "Ks" :: r :: g :: b :: [] -> {mat with mat_spec = rgb r g b}
      | "Ns" :: x :: [] -> {mat with mat_shiny = fos x}
      | "Km" :: x :: [] -> {mat with mat_km = fos x}
      | "d" :: x :: [] -> {mat with mat_trans = fos x}
      | "r" :: x :: [] -> {mat with mat_reflect = fos x}
      | "sharpness" :: x :: [] -> {mat with mat_glossy = fos x}
      | "Ni" :: x :: [] -> {mat with mat_refract_index = fos x}
      | "map_Ka" :: str :: [] -> {mat with mat_mapka = str}
      | "map_Kd" :: str :: [] -> {mat with mat_mapkd = str}
      | "map_D" :: str :: [] -> {mat with mat_mapd = str}
      | "map_Bump" :: str :: [] -> {mat with mat_mapbump = str}
      | "illum" :: x :: [] -> {mat with mat_illum = ios x}
      | tok :: rest -> raise (ParsingWarning ("Unknown token '" ^ tok ^ "'"))
      | _ -> mat
    else
      mat in

  (* open the file and recursively build all matlist *)
  let warn = ref [] and err = ref [] in
  let chan = open_in filename in
  let rec process_mtl chan lineno mat =
    match read_line chan with
      Some x ->
        let newmat = (try do_mtl_line mat x lineno with
          ParsingError str -> append_error_warning err filename str lineno; mat
        | ParsingWarning str -> append_error_warning warn filename str lineno; mat) in
        process_mtl chan (lineno + 1) newmat
    | None -> mat in

  (* first material will be default one *)
  append (process_mtl chan 1 default_material);

  (* output complete diagnostics *)
  Printf.fprintf stderr "file: '%s'\n" filename;
  report_error_warning "warnings" !warn;
  report_error_warning "errors" !err;

  (* something bad happened: do not build anything *)
  if List.length !err > 0 then
    raise (ParsingError ("failed to parse '" ^ filename ^ "'"));

  (* build a map between material name and index (be careful: reversed list *)
  let len = List.length !matlist in
  let matmap = Hashtbl.create len in
  List.iteri (fun index m -> htadd matmap m.mat_name (len-index-1)) !matlist;
  (!matlist, matmap)

(* make an array from a *reversed* list *)
let makearray l default =
  let len = List.length l in
  if len = 0 then [||]
  else
    let arr = Array.make len default in
      List.iteri (fun index x -> arr.(len-index-1) <- x) l;
    arr

(* parse a 3d vector made of x y z *)
let do_vec3 lst tokens =
  match tokens with
    x :: [] -> lst := {x = fos x; y = 0.0; z = 0.0} :: !lst
  | x :: y :: [] -> lst := {x = fos x; y = fos y; z = 0.0} :: !lst
  | x :: y :: z :: [] -> lst := {x = fos x; y = fos y; z = fos z} :: !lst
  | _ -> raise (ParsingError "improperly formed 3d vector")

let match_vert_pn_re = Str.regexp "[0-9]+//[0-9]+"
let split_vert_pn_re = Str.regexp "//"
let split_vert_re = Str.regexp "/"
let improper_vertex_index = ParsingError "improperly formed vertex index"

(* append a new vertex in the vertex map. much slower than C strstr version *)
let do_vertex vertices vertn str =
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
  try htfind vertices (non_negative v) with Not_found -> htadd vertices v !vertn;
  incr vertn;
  !vertn - 1

(* a face is made of variable number of vertices *)
let do_face tris vertices vertn currmat f =
  let append v0 v1 v2 =
    tris := {
      tri_vert = {xi = v0; yi = v1; zi = v2};
      tri_mat = !currmat
    } :: !tris in
  let add = do_vertex vertices vertn in
  match List.map add f with
    v0 :: v1 :: v2 :: [] -> append v0 v1 v2
  | v0 :: v1 :: v2 :: v3 :: [] -> append v0 v1 v2; append v0 v2 v3
  | _ -> raise (ParsingError "only triangles and quads are suported for now")

(* parse the wavefront obj file *)
let loadobj filename =

  let pos = ref [default_pos] in (* list of positions *)
  let nor = ref [default_nor] in (* list of all normal vectors *)
  let tex = ref [default_tex] in (* list of all texture coordinates *)
  let vertices = Hashtbl.create 0 in (* contains indexed .obj vertices *)
  let tris = ref [] in (* list of indexing triangles *)
  let allmat = ref [] in (* list of materials found in .mtl files *)
  let matmap = ref (Hashtbl.create 0) in (* string -> material index *)
  let currmat = ref 0 in (* current material *)
  let vertn = ref 0 in (* number of generated vertices *)

  (* define the process done line by line for an .obj file *)
  let do_obj_line line =
    match split line with
      "v" :: rest -> do_vec3 pos rest
    | "vn" :: rest -> do_vec3 nor rest
    | "vt" :: rest -> do_vec3 tex rest
    | "f" :: rest -> do_face tris vertices vertn currmat rest
    | "mtllib" :: name :: [] -> (match loadmtl name with (mat, map) -> allmat := mat; matmap := map)
    | "usemtl" :: name :: [] -> currmat := (try htfind !matmap name with Not_found -> 0)
    | _ -> () (* silently ignore the rest, including comments and groups *) in

  (* open the file and parse it line by line *)
  let warn = ref [] and err = ref [] in
  let chan = open_in filename in
  let rec process_obj chan lineno =
    match read_line chan with
      Some x -> (try do_obj_line x with
          ParsingError str -> append_error_warning err filename str lineno
        | ParsingWarning str -> append_error_warning warn filename str lineno);
        process_obj chan (lineno + 1)
    | None -> () in
  process_obj chan 1;

  (* output complete diagnostics *)
  Printf.fprintf stderr "file: '%s'\n" filename;
  report_error_warning "warnings" !warn;
  report_error_warning "errors" !err;
  if List.length !err > 0 then raise (Failure ("failed to parse '" ^ filename ^ "'"))

  (* no error: we build the final obj structure *)
  allmat := if !allmat = [] then [default_material] else !allmat;

  (* triangle and material arrays are simply built from their respective lists *)
  let tri_array = makearray !tris {tri_mat = 0; tri_vert = {xi = 0; yi = 0; zi = 0}} in
  let mat_array = makearray !allmat default_material in

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
  { triangles = tri_array; vertices = vert_array; materials = mat_array }

