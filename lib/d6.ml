open Utils
open Printf
open Unix

(*TSIA*)

let time_execution f =
  let start_time = gettimeofday () in
  let result = f () in
  let end_time = gettimeofday () in
  Printf.printf "Execution time: %.6f seconds\n" (end_time -. start_time);
  result

module TupleSet = Set.Make(struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
end)

let filename = "inputs/toasty.txt";;

type matrix = char array array;;
type matrix_dimensions = int * int;;
type direction = North | South | West | East;;
type position = int * int;;
type pos_dir = {position: position; direction: direction}
type state = Running | Loop | Break;;

let print_pos_dir (posdir : pos_dir) = 
    match posdir.direction with 
        | North -> printf "position: (%d, %d) direction: North\n" (fst posdir.position) (snd posdir.position)
        | South -> printf "position: (%d, %d) direction: South\n" (fst posdir.position) (snd posdir.position)
        | East -> printf "position: (%d, %d) direction: East\n" (fst posdir.position) (snd posdir.position)
        | West -> printf "position: (%d, %d) direction: West\n" (fst posdir.position) (snd posdir.position);;

let print_position (position: position) = 
    printf "pos: (%d, %d) \n\n\n" (fst position) (snd position);;

let get_next_pos (posdir : pos_dir) = 
    match posdir.direction with 
        | North -> {position = ((fst posdir.position - 1), (snd posdir.position )); direction = posdir.direction}
        | South -> {position = ((fst posdir.position + 1), (snd posdir.position)); direction = posdir.direction}
        | East -> {position = ((fst posdir.position), (snd posdir.position + 1)); direction = posdir.direction}
        | West -> {position = ((fst posdir.position), (snd posdir.position - 1)); direction = posdir.direction};;

let peek_for_block (matrix : matrix) (pos: int * int) : bool = 
    if matrix.(fst pos).(snd pos) = '#' 
    then true 
    else false;;

let find_position (matrix : matrix) (target : char array) : (int * int) option =
  let result = ref None in
  Array.iteri (fun row_index row ->
    Array.iteri (fun col_index element ->
      if Array.exists (fun x -> element = x) target then result := Some (row_index, col_index)
    ) row;
  ) matrix;
  !result;;

let place_mark (matrix : matrix) (target : position) (mark : char) = matrix.(fst target).(snd target) <- mark;;

let turn = function
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North;;

let determine_direction = function
    | '^' -> Some North
    | 'v' -> Some South
    | '>' -> Some East
    | '<' -> Some West
    | _ -> None;;


let get_matrix_dimensions (matrix: matrix): matrix_dimensions = (Array.length matrix - 1, Array.length matrix.(0) - 1);;
let print_matrix_len (dimensions: matrix_dimensions) = printf "Matrix dimensions: %d x %d\n" (fst dimensions) (snd dimensions);;
let print_matrix (matrix: matrix) = Array.iter (fun outer -> (Array.iter (printf "%c") outer); printf "\n" )matrix;;

let pos_is_inbound (dimensions: matrix_dimensions) (new_pos: position) = 
    if fst new_pos <= fst dimensions 
    && snd new_pos <= snd dimensions
    && fst new_pos >= 0
    && snd new_pos >= 0
      then true else false;;

let place_mark (matrix : matrix) (target : position) (mark : char) = 
    matrix.(fst target).(snd target) <- mark;;

let place_mark_ignore_start (matrix : matrix) (target : position) (mark : char) (init_pos_dir: pos_dir) = 
    if target <> init_pos_dir.position then
    matrix.(fst target).(snd target) <- mark;;

let count_marks (matrix: matrix) =
  Array.fold_left (fun acc row ->
    acc + Array.fold_left (fun acc x -> if x = 'X' then acc + 1 else acc) 0 row
  ) 0 matrix;;

let marks_to_set (matrix: matrix) (mark: char): TupleSet.t =
  Array.fold_left (fun acc row_index ->
    Array.fold_left (fun acc (col_index, cell) ->
      if cell = mark then TupleSet.add (row_index, col_index) acc else acc
    ) acc (Array.mapi (fun col_index cell -> (col_index, cell)) matrix.(row_index))
  ) TupleSet.empty (Array.init (Array.length matrix) Fun.id);;


let walk_and_mark_map (matrix: matrix) (starting_point: pos_dir) = 
    let dimensions = get_matrix_dimensions matrix in
    let rec aux acc pos = 
        match acc with 
        | Some x  -> x
        | None -> 
            place_mark matrix pos.position 'X'; 
            if not (pos_is_inbound dimensions (get_next_pos pos).position) then aux (Some matrix) pos else
            if peek_for_block matrix (get_next_pos pos).position
            then aux None {position = pos.position; direction = turn pos.direction} 
            else aux None {position = (get_next_pos pos).position; direction = pos.direction} in
    aux None starting_point;;

let been_here_before (pos: position)(break_set: TupleSet.t) = 
    if TupleSet.mem pos break_set then true else false;;

let rec peek_turn_and_go (matrix: matrix) (current_pos_dir: pos_dir) = 
    let is_guard = peek_for_block matrix (get_next_pos current_pos_dir).position in
    match is_guard with 
    | true -> peek_turn_and_go matrix {position = current_pos_dir.position; direction = turn current_pos_dir.direction}
    | false -> {position = (get_next_pos current_pos_dir).position; direction = current_pos_dir.direction};;

let detect_loop (matrix: matrix) (starting_point: pos_dir) = 
    let dimensions = get_matrix_dimensions matrix in
    let rec aux acc pos_dir break_set = 
        match acc with 
        | Break   -> Break
        | Loop    -> Loop
        | Running -> 
            if not (pos_is_inbound dimensions (get_next_pos pos_dir).position) 
            then aux Break pos_dir break_set 
            else
                if peek_for_block matrix (get_next_pos pos_dir).position
                then if been_here_before pos_dir.position break_set 
                     then aux Loop { position = pos_dir.position; direction = turn pos_dir.direction } break_set
                     else aux Running (peek_turn_and_go matrix pos_dir) (TupleSet.add pos_dir.position break_set)
                else aux Running (peek_turn_and_go matrix pos_dir)  break_set 
    in
    aux Running starting_point TupleSet.empty;;

let main () =
  let matrix = read_lines filename 
               |> List.filter (fun x -> x <> "")
               |> List.map string_to_char_list 
               |> List.map Array.of_list
               |> Array.of_list in
  
  let matrix2 = Array.copy matrix in

  let initial_position_option = find_position matrix [|'^'; 'v';'>';'<'|] in

  let initial_position = match initial_position_option with
                          | Some x -> x
                          | None -> failwith "Initial Position Failed" in

  let initial_direction_option = determine_direction matrix.(fst initial_position).(snd initial_position) in
  let initial_direction = match initial_direction_option with
                          | Some x -> x
                          | None -> failwith "Initial Direction Failed" in

  let init_pos_dir = {position = initial_position; direction = initial_direction} in
  let marked_map = walk_and_mark_map matrix2 init_pos_dir in
  let mark_set = marks_to_set marked_map 'X' in
  let result = TupleSet.fold (fun x acc ->
      place_mark_ignore_start matrix x '#' init_pos_dir;
      let is_loopy = detect_loop matrix init_pos_dir in
          match is_loopy with
          | Loop -> place_mark_ignore_start matrix x '.' init_pos_dir; acc + 1
          | Break -> place_mark matrix x '.'; acc
          | _ -> failwith "Should never be here"
    ) mark_set 0 in
  Printf.printf "Result: %d\n" result

let () = time_execution main
