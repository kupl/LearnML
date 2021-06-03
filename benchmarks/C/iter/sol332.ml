(* problem 3*)
let fun_add f1 f2 = fun x -> f2(f1 x)
let rec fun_adds n f = if n=0 then fun x->x else fun_add f (fun_adds (n-1) f) 

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun_adds n f