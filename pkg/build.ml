#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () = Pkg.describe "tlstools" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "lib/tlstools";
    Pkg.bin ~auto:true "bin/tlsvis";
    Pkg.bin ~auto:true "bin/tlsweb";
    Pkg.doc "README.md"; ]
