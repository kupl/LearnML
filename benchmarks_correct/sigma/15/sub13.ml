
let rec sigma f a b  =
    if a > b then 0
    else if a == b then f(a)
    else f(a) + sigma f (a+1) b

(*
let _ = print_endline(string_of_int(sigma(2,10,(fun x->x+1))))
*)
