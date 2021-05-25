(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> if n = 0 then fun x -> x
                else let main_func = f 
                     in let rec help_iter n f = if n = 1 then f
                                                else help_iter (n-1) (fun x -> main_func(f(x)))
                        in (help_iter n f)