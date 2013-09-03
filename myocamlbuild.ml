open Ocamlbuild_plugin;;
flag ["ocaml"; "compile"; "native"] (A "-S");;
flag ["ocaml"; "compile"; "native"] (A "-noassert");;
flag ["ocaml"; "compile"; "native"] (A "-unsafe");;
flag ["ocaml"; "compile"; "native"] (A "-inline");;
flag ["ocaml"; "compile"; "native"] (A "100");;

flag ["ocaml"; "compile"; "byte"] (A "-g");;
flag ["ocaml"; "link"; "byte"] (A "-g");;

