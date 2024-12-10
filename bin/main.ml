open My_lib.Utils
open My_lib.D6
open Fmt

type person = { name : string; age : int } [@@deriving show]
type knuti = { knuti : string; age : int } [@@deriving show]

let () =
  let alice = { name = "Alice"; age = 30 } in
  let knuti = { knuti = "knuti"; age = 1337 } in
  pr "%a\n" pp_person alice; pr "%a\n" pp_knuti knuti;
