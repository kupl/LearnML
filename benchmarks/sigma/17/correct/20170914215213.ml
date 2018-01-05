let rec sigma (a, b, f) = 
    if b < a then 0
    else f(b) + sigma(a, b-1, f)

(*
let sum1 = sigma(1,10,fun x -> x)

let sum2 = sigma(1,10,fun x->x*x)
let sum3 = sigma(-10,-1,fun x->2*x-1)

let sum4 = sigma(3,2,fun x->2*x-1)

let _ = print_endline(string_of_int sum1)
let _ = print_endline(string_of_int sum2)
let _ = print_endline(string_of_int sum3)

let _ = print_endline(string_of_int sum4)

*)
