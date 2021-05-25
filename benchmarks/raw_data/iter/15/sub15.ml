
let rec iter : (int * ('a -> 'a)) -> ('a -> 'a) = fun(n, f) ->
    if n == 0 then fun x->x
    else fun x->(iter(n-1,f)(f(x)))

(*
let _ = print_endline(string_of_int(iter(3, function x->2+x) 0))
let _ = print_endline(string_of_int(iter(5, function x->2+x) 0))

let _ = print_endline (string_of_float(iter(3, function x->x*.x) 1.2))
*)
