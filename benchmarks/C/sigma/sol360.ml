let rec sigma f a b  =
        if a>b then 0
        else f(b) + sigma f a (b-1)
(*
let f : int -> int = fun x -> x+3
let a : int = sigma(0,8,f)
let _ = print_endline(string_of_int a)


let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
print_bool (0 = sigma (3, 1, fun x -> x * x)); 
print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
*)
