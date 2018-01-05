
let rec sigma : int*int*(int->int) -> int = fun(a, b, f) ->
    if a > b then 0
    else if a == b then f(a)
    else f(a) + sigma(a+1, b, f)

(*
let _ = print_endline(string_of_int(sigma(2,10,(fun x->x+1))))
*)
