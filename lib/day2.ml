open Utils

let rec part_one distance depth = function
    | [] -> (distance, depth)
    | h :: t -> match fst h with 
                | "up" -> part_one distance (depth - snd h) t
                | "down" -> part_one distance (depth + snd h) t
                | "forward" -> part_one (distance + snd h) depth t
                | _ -> part_one distance depth t;;

let rec part_two distance depth aim = function
    | [] -> (distance, depth)
    | h :: t -> match fst h with 
                | "up" -> part_two distance depth ( aim - snd h) t
                | "down" -> part_two distance depth ( aim + snd h) t
                | "forward" -> part_two (distance + snd h) ( depth + (aim * snd h) ) aim t
                | _ -> part_two distance depth aim t;;

let solution f filename = read_lines filename
    |> List.map (fun x -> String.split_on_char ' ' x)
    |> List.map (function 
        | [_] -> None 
        | [a ; b] -> Some(a, int_of_string b)
        | _ -> None)
    |> List.filter_map(fun x -> x)
    |> f;;

let time_part_one filename = time_function (fun () -> solution (part_one 0 0) filename);;
let time_part_two filename = time_function (fun () -> solution (part_two 0 0 0) filename);;
