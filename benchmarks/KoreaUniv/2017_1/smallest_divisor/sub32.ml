(* problem 2*)

let rec smallest_divisor_rec i n =
        if i*i > n then n
        else if n mod i = 0 then i
        else smallest_divisor_rec (i+1) n

let smallest_divisor : int -> int
= fun n -> smallest_divisor_rec 2 n