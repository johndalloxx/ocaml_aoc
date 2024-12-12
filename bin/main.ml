open My_lib.Utils
open My_lib.D6
open Fmt

type person = { name : string; age : int } [@@deriving show]
type knuti = { knuti : string; age : int } [@@deriving show]
type matrix = char array array [@@deriving show]

  (*let alice = { name = "Alice"; age = 30 } in*)
  (*let knuti = { knuti = "knuti"; age = 1337 } in*)
  (*pr "%a\n" pp_person alice; pr "%a\n" pp_knuti knuti;*)
let bool_to_string = function
  | true -> "true"
  | false -> "false"
;;
let idx_of_first (array: char array) (input: char array) = 
    let lookup_first = input.(0) in
    let idx = Array.find_index (fun x -> x = lookup_first) array in
    match idx with | Some x -> x | None -> -1;;

let contains_input (array: char array) (input: char array)  = 
    Array.for_all2 (fun a b -> a = b) array input;;

let search_from_idx (array: char array) (look_up: char array) (idx: int) = 
    let look_up_len = Array.length look_up in
    let sub_array = Array.sub array idx look_up_len in
    if contains_input sub_array look_up then true else false;;

let () = 
    let matrix = read_lines "inputs/day4t-2024.txt" 
        |> List.filter ((<>) String.empty)
        |> List.map string_to_char_list 
        |> List.map Array.of_list
        |> Array.of_list in
    Array.iter (pr "%a\n" (brackets (array ~sep:(const string ",") char))) matrix;;
    
    let input_array  = [|'X'; 'M'; 'A'; 'S'|] in
    let search_array = [|'M'; 'A'|] in
    let idx = idx_of_first input_array search_array in
    Printf.printf "%s\n" (bool_to_string (search_from_idx input_array search_array idx));;
