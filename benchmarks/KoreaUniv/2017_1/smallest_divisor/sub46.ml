(* problem 2*)
let rec help_smallest_divisor : int -> int -> int
= fun n a -> if float_of_int a > sqrt(float_of_int n) then n
             else if n mod a = 0 then a
             else (help_smallest_divisor n (a + 1))