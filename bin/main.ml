open My_lib.Utils
open My_lib.D6
open Fmt

type matrix_arr = char array array [@@deriving show]
type matrix_lst = char list list [@@deriving show]
type dimensions = int * int
type point = int * int
type str_and_len = string * int
type charr_and_len = char array * int

let dimensions matrix: dimensions =
  let rows = Array.length matrix in
  let cols = if rows > 0 then Array.length matrix.(0) else 0 in
  (rows, cols);;

let diagonals matrix =
  let rows = Array.length matrix in
  let cols = if rows > 0 then Array.length matrix.(0) else 0 in
  let max_diag = rows + cols - 1 in
  let result = Array.make max_diag [] in

  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let diag_index = row - col + (cols - 1) in
      result.(diag_index) <- matrix.(row).(col) :: result.(diag_index)
    done
  done;

  Array.map List.rev result |> Array.to_list;;

let anti_diagonals matrix =
  let rows = Array.length matrix in
  let cols = if rows > 0 then Array.length matrix.(0) else 0 in
  let max_diag = rows + cols - 1 in
  let result = Array.make max_diag [] in

  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let diag_index = row + col in
      result.(diag_index) <- matrix.(row).(col) :: result.(diag_index)
    done
  done;
  Array.map List.rev result |> Array.to_list;;

let search_from_idx (arr, _: charr_and_len) (word, w_len: charr_and_len) (idx: int) = 
    let sub_array = Array.sub arr idx w_len in
    if Array.for_all2 (fun a b -> a = b) sub_array word 
    then Some idx 
    else None;;

let find_substr (input, i_len: str_and_len) (word, w_len: str_and_len) = 
    range 0 (i_len - w_len + 1) 
    |> Seq.map (fun idx -> 
            search_from_idx (str_to_array input, i_len ) (str_to_array word, w_len) 
            idx)
    |> List.of_seq
    |> List.filter_map Fun.id;;

let is_mas = function
    | "SAM" -> true
    | "MAS" -> true
    | _ -> false;;

let get_cross (matrix: matrix_arr) (y, x: point) = 
  let rows = Array.length matrix in
  let cols = if rows > 0 then Array.length matrix.(0) else 0 in

  if y <= 0 || y >= rows - 1 || x <= 0 || x >= cols - 1 then
    false
  else

  let res1 = Array.make 3 ' ' in
  let res2 = Array.make 3 ' ' in

  for i = 0 to 2 do
      res1.(i) <- matrix.(y - 1 + i).(x - 1 + i)
  done;

  for i = 0 to 2 do
      res2.(i) <- matrix.(y - 1 + i).(x + 1 - i)
  done;

  if (is_mas (str_of_array res1)) && (is_mas (str_of_array res2))
  then true else false;;


let find_point_by_char (c: char) (matrix: matrix_lst)= 
        List.mapi (fun idy y -> 
            List.mapi (fun idx x -> if x = c 
                                    then Some (idy, idx) 
                                    else None) 
        y) matrix;;

let filename = "inputs/day4-2024.txt"

let lines = 
  read_lines filename
  |> List.filter (fun line -> line <> "")

let input =
  lines
  |> List.map (fun line -> line |> string_to_char_list |> Array.of_list)
  |> Array.of_list


let regular = 
  input 
  |> Array.to_list 
  |> List.map Array.to_list

let pos_of_A = 
    regular 
    |> find_point_by_char 'A'
    |> List.map (List.filter_map Fun.id)
    |> List.flatten

let mas_cross_points = 
    pos_of_A 
    |> List.filter (fun x -> get_cross input x)

let mac_cross_point_count = 
    mas_cross_points
    |> List.length

let reversed = 
  regular 
  |> List.map List.rev

let diagonal = diagonals input
let anti_diagonal = anti_diagonals input

let rev_diagonal = 
  diagonal 
  |> List.map List.rev

let rev_anti_diagonal = 
  anti_diagonal 
  |> List.map List.rev

let transposed = 
  regular 
  |> transpose

let transposed_rev = 
  transposed 
  |> List.map List.rev

let all_sequences = 
  List.concat [
    regular; reversed; 
    diagonal; anti_diagonal; 
    rev_diagonal; rev_anti_diagonal; 
    transposed; transposed_rev
  ]

let as_strings =
  all_sequences
  |> List.map (fun chars -> 
       chars |> List.to_seq |> String.of_seq
     )

let matches =
  as_strings
  |> List.map (fun s -> find_substr (s, String.length s) ("XMAS", 4))

let non_empty_matches =
  matches 
  |> List.filter (fun occs -> not (List.is_empty occs))

let total_occurrences =
  non_empty_matches
  |> List.fold_left (fun acc occs -> acc + List.length occs) 0

let () = Printf.printf "Number of occurrences: %d\n" total_occurrences
let () = Printf.printf "%d\n" mac_cross_point_count;;
