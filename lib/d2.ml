open Utils

let filename = "day2-2024.txt";;
let id x = x;;
type direction = Increasing | Decreasing | Neither | NA;;

let rec is_safe = function
    | [] -> true
    | [_] -> true
    | [h; t] -> if abs (h - t) > 3 then false else true
    | f :: s :: r -> if abs (f - s) > 3 then false else is_safe (s :: r);;

let find_direction x y = if x = y then Neither else if x < y then Increasing else Decreasing;;

let rec is_madness acc = function
    | [] -> true
    | [_] -> true
    | [h; t] -> if find_direction h t == acc then true else false
    | f :: s :: r -> 
            if acc == NA then is_madness (find_direction f s) (f :: s :: r) else
                if find_direction f s == acc 
                then is_madness acc (s :: r) 
                else false;;

let remove_thingy idx lst = lst |> List.mapi  (fun midx x -> if midx = idx then None else Some x)  |> List.filter_map id;;

let thingy = read_lines filename 
    |> List.map(String.split_on_char ' ') 
    |> List.map(List.map int_of_string_opt)
    |> List.map(List.filter_map(id))
    |> List.filter(fun x -> not (List.is_empty x))
    |> List.map(fun x -> remove_thingy 0 x)

    (*|> List.map(fun x -> is_safe x && is_madness NA x)*)
    (*|> List.filter(id)*)
    (*|> List.length *)
    (*|> printf "%d"*)
