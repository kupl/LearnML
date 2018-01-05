let rec merge ((x : int list), (y : int list)) : int list = 
    match (x, y) with
    | ([], a) -> a
    | (a, []) -> a
    | (hd1 :: tl1 , hd2 :: tl2) ->
            if (hd1 > hd2) then hd1 :: merge (tl1, y)
            else hd2 :: merge(x, tl2)
 
let a = [5;3;1]
let b = [6;4;2]
open Printf
let _ = List.iter (printf "%d ") (merge (a, b));print_newline()

let _ = print_endline (string_of_bool ([6;5;4;3;2;1] = merge(a, b)))
