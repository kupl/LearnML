let rec iter (n ,f) a =
    match n with
    | 0 -> a
    | _ -> (iter (n - 1, f) (f a))


(*let a31 = iter (3, function x -> 2+x) 0
let a32 = iter (0, function x -> 2*x) 4
let a33 = iter (11, function x -> 2*x+1) 7 

let _ = print_endline(string_of_int a31)
let _ = print_endline(string_of_int a32)
let _ = print_endline(string_of_int a33)*)
