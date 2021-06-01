let rec sigma f a b =
    match (a ,b) with
    | (x, y) ->
    	if x = y then f x
    	else if x > y then 0
    	else f x + sigma f (x+1) y

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
