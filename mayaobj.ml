module MayaObj =
  struct
    type vec2d = { x:float; y:float; }
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
      amb:vec3d;
      diff:vec3d;
      spec:vec3d;
    }

    (* indexing triangle *)
    type triangle = {
      v:vec3i;
      m:int;
    }

    (* complete vertex *)
    type vertex = {
      p:vec3d;
      n:vec3d;
      t:vec2d;
    }

    (* describe consecutive triangles with same material *)
    type matgroup = {
      first:int;
      last:int;
      m:int;
    }

    (* complete wavefront obj mesh *)
    type obj = {
      tri:triangle array;
    }

    (* read a file and output a list of lines *)
    let read_file filename = 
      let lines = ref [] in
      let chan = open_in filename in
      try
        while true; do
          lines := input_line chan :: !lines
        done; []
      with End_of_file ->
        close_in chan;
        List.rev !lines
    ;;

    (* temporary structure while parsing the wavefront obj *)
    let loadobj filename =
      let vertexmap = Hashtbl.create 1 in (* contains all obj vertices *)
      let vertices = [] in (* *)
      let chan = open_in filename in
      try
        while true; do
          let line = input_line chan in
          let tokens = Str.split (Str.regexp "[\t\ ]+") line in
          let parse_vertex v = [] in
          let rec parse_line l = match l with
            "v" :: rest -> parse_line (parse_vertex rest)
            | _ -> ()
          in ()
        done;
      with End_of_file ->
        close_in chan
    ;;
  end;;

