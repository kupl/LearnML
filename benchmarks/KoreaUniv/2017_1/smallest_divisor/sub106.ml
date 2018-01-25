(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let est = 3 in
           let rec f n est = if n mod 2 == 0 then 2
                             else if est*est > n then n
                             else if n  mod est == 0 then est 
                             else f n (est+2) in
           f n est