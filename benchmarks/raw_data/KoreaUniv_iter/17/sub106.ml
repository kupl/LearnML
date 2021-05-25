(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun x -> let rec g (n, f) = if n == 0 then x else f(g (n-1, f)) in
               g (n, f) 