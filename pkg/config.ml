#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "tlstools";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "Hannes Mehnert <hannes\\@mehnert.org>" ]
end
