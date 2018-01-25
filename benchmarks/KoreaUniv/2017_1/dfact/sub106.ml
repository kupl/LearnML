(* problem 5*)

let dfact : int -> int
= fun n -> let rec f n = if n == 1 || n == 2 then n 
                         else n*f (n-2) in
           f n