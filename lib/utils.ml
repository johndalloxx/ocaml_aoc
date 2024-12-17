let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents;;

let range start stop =
    Seq.unfold (fun current -> 
        if current >= stop then None else Some (current, current + 1)
    ) start;;

let parse_input filename = List.map int_of_string_opt (read_lines filename) |> List.filter_map(fun x -> x);;

let str_to_array str =
    let seq = String.to_seq str in
    Array.of_seq seq;;

let str_of_array (arr: char array): string = 
    Bytes.init (Array.length arr) (fun x -> arr.(x)) |>
    Bytes.to_string;;

let bool_to_string = function
  | true -> "true"
  | false -> "false"
;;

let rec transpose = function
  | [] -> [] 
  | [] :: _ -> [] 
  | matrix ->
    let first_column = List.filter (fun row -> row <> []) matrix |> List.map List.hd in
    let rest_matrix = List.filter (fun row -> row <> []) matrix |> List.map List.tl in
    first_column :: transpose rest_matrix

let rec find_diagonal = function 
  | [] -> []
  | [] :: _ -> []
  | row :: rest -> 
      let first_element_first_row = List.hd row in
      let rest_without_head = 
        List.filter (fun r -> r <> []) rest 
        |> List.map List.tl 
      in
      first_element_first_row :: find_diagonal rest_without_head

let all_diagonals lst = 
  let rec aux acc = function
    | [] -> List.rev acc  (* Reverse to maintain the original order *)
    | h :: t -> 
        let diag = find_diagonal (h :: t) in
        aux (diag :: acc) t
  in
  aux [] lst;;

let find_substring haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec aux i =
    if i > haystack_len - needle_len then None
    else if String.sub haystack i needle_len = needle then Some i
    else aux (i + 1)
  in
  aux 0;;

let string_to_char_list s =
  List.of_seq (String.to_seq s)

let time_function f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Execution time: %.6f seconds\n" elapsed_time;
  result
