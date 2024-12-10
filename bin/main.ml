open My_lib.Utils
open My_lib.D6
open Fmt

type person = { name : string; age : int } [@@deriving show]
type knuti = { knuti : string; age : int } [@@deriving show]
type matrix = char array array

  (*let alice = { name = "Alice"; age = 30 } in*)
  (*let knuti = { knuti = "knuti"; age = 1337 } in*)
  (*pr "%a\n" pp_person alice; pr "%a\n" pp_knuti knuti;*)

let () = 
    let matrix = read_lines "inputs/day4t-2024.txt" 
        |> List.filter ((<>) String.empty)
        |> List.map string_to_char_list 
        |> List.map Array.of_list
        |> Array.of_list in

    Array.iter (fun x -> Printf.printf "%d\n" (Array.length x)) matrix;
    Array.iter (pr "%a\n" (brackets (array ~sep:(const string ",") char))) matrix;;
