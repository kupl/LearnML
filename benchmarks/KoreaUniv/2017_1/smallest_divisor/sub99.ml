(* problem 2*)

let rec smallest_divisor : int -> int
= fun n -> if n <= 1 then raise(Failure "Error") else
let rec sml n x = match (n mod x) with
|0 -> x
|_ -> if(x * x > n) then n else sml n (x+1) in
 sml n 2