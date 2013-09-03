(* vectors *)
type vec3d = { x : float; y : float; z : float }
type vec3i = { xi : int; yi : int; zi : int }

(* material description *)
type material = {
  mat_name: string;
  mat_mapka: string;
  mat_mapkd: string;
  mat_mapd: string;
  mat_mapbump: string;
  mat_km: float;
  mat_reflect: float;
  mat_refract: float;
  mat_trans: float;
  mat_shiny: float;
  mat_glossy: float;
  mat_refract_index: float;
  mat_illum: int;
  mat_amb: vec3d;
  mat_diff: vec3d;
  mat_spec: vec3d;
}

(* indexed triangle *)
type triangle = {
  tri_mat: int;
  tri_vert: vec3i;
}

(* complete vertex *)
type vertex = {
  p: vec3d;
  n: vec3d;
  t: vec3d;
}

(* complete wavefront obj mesh *)
type obj = {
  triangles: triangle array;
  vertices: vertex array;
  materials: material array;
}

