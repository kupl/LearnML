let rec merge (l1, l2) : int list =
    match (l1, l2) with
    | ([], _) -> l2
    | (_, []) -> l1
    | (hd1::tl1, hd2::tl2) when hd1 > hd2 -> hd1::(merge (tl1, l2))
    | (hd1::tl1, hd2::tl2) -> hd2::(merge (l1, tl2))

(*let a11 = merge ([7; 2; 1], [5; 4; 3])
let a12 = merge ([], [])
let a13 = merge ([9; 2], [])
let a14 = merge ([], [7; 3])
let a15 = merge ([5; 4; 3], [5; 4; 3])
let a16 = merge ([5; 3; 1], [8; 6; 4; 2; 0]) 

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let _ = print_list a11 
let _ = print_endline " "
let _ = print_list a12 
let _ = print_endline " "
let _ = print_newline
let _ = print_list a13 
let _ = print_endline " "
let _ = print_newline
let _ = print_list a14 
let _ = print_endline " "
let _ = print_newline
let _ = print_list a15
let _ = print_endline " "
let _ = print_newline
let _ = print_list a16
let _ = print_endline " "
let _ = print_newline *)
