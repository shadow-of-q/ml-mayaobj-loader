open Mayaobjparser
open Mayaobj

let scene = loadobj "conference.obj";;

Printf.printf "vertices %d\ntriangles %d\nmaterials %d\n"
  (Array.length scene.vertices)
  (Array.length scene.triangles)
  (Array.length scene.materials)

