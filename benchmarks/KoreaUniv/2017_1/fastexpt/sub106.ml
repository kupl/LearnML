(* problem 1*)

let fastexpt : int -> int -> int
= fun b n -> let rec f b n = if n == 0 then 1
                             else if n mod 2 == 0 then let x = f b (n/2) in x*x
                             else b*f b (n-1) in
             f b n