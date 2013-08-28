(* vectors *)
type vec3d = { x:float; y:float; z:float }
type vec3i = { xi:int; yi:int; zi:int }

(* material description *)
type material = {
  name:string;
  mapka:string;
  mapkd:string;
  mapd:string;
  mapbump:string;
  km:float;
  reflect:float;
  refract:float;
  trans:float;
  shiny:float;
  glossy:float;
  refract_index:float;
  illum:int;
  amb:vec3d;
  diff:vec3d;
  spec:vec3d;
}

(* indexed triangle *)
type triangle = {
  m:int;
  v:vec3i;
}

(* complete vertex *)
type vertex = {
  p:vec3d;
  n:vec3d;
  t:vec3d;
}

(* describe consecutive triangles with same material *)
type matgroup = {
  first:int;
  last:int;
  m:int;
}

(* complete wavefront obj mesh *)
type obj = {
  triangles:triangle array;
  vertices:vertex array;
  materials:material array;
}

