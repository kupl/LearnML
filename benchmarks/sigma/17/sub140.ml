let rec sigma (a, b, f) : int = 
    match (a ,b) with
    | (x, y) when x = y -> f x
    | (x, y) when x > y -> 0
    | (x, y) -> f x + sigma (x + 1, y, f)

(*let a21 = sigma (0, 3, function x -> x)
let a22 = sigma (3, 3, function x -> x)
let a23 = sigma (7, 10, function x -> x*x)
let a24 = sigma (11, 10, function x -> x*x)

let _ = print_int(a21)
let _ = print_endline("")
let _ = print_int(a22)
let _ = print_endline("")
let _ = print_int(a23)
let _ = print_endline("")
let _ = print_int(a24)
let _ = print_endline("")*)
