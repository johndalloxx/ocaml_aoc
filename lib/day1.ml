open Utils

let rec some_sliding_windowy_thing acc = function
    | [] | [_] | [_ ; _] -> acc
    | first :: second :: third :: rest ->
            (first , second , third) :: (some_sliding_windowy_thing acc (second::third::rest));;

let rec count_increasing_numbers acc = function 
    | [] | [_] -> acc
    | first :: second :: rest -> 
            if first < second 
            then count_increasing_numbers (acc + 1) (second :: rest) 
            else count_increasing_numbers acc (second :: rest);;

let partOne filename = parse_input filename |> count_increasing_numbers 0;;

let partTwo filename = parse_input filename 
    |> some_sliding_windowy_thing [] 
    |> List.map (fun (a, b, c) -> a + b + c) 
    |> count_increasing_numbers 0;;
