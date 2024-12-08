open Utils

let filename = "123";;

let count_occurrences lst =
  let table = Hashtbl.create 20 in
  List.iter (fun x ->
    if Hashtbl.mem table x then
      Hashtbl.replace table x (Hashtbl.find table x + 1)
    else
      Hashtbl.add table x 1
  ) lst;
  table

let columns = read_lines filename 
    |> List.map (fun line -> String.split_on_char ' ' line) 
    |> List.map (fun x -> List.filter (fun x -> x <> String.empty) x);;

let first_col = List.map (function | [f;_] -> Some(int_of_string f) | _ -> None) columns 
    |> List.filter_map(fun x -> x);;

let second_col = List.map (function | [_;s] -> Some(int_of_string s) | _ -> None) columns 
    |> List.filter_map(fun x -> x);;

let first_col_sorted = first_col |> List.sort compare;;
let second_col_sorted = second_col |> List.sort compare;;
let second_col_hashtbl = second_col |> count_occurrences;;

let part_one = List.combine first_col_sorted second_col_sorted
    |> List.fold_left (fun acc x -> 
            if fst x < snd x
            then acc + snd x - fst x
            else acc + fst x - snd x
            ) 0;;

let part_two = first_col 
    |> List.fold_left (fun acc x -> match Hashtbl.find_opt second_col_hashtbl x with
    | None -> acc
    | Some y -> acc + y * x) 0
;;

Printf.printf "total_distance: %d\n" part_one;;
Printf.printf "similarity_score: %d\n" part_two;;
