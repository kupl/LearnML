(* problem 1*)

let check_even n = if (n/2)+(n/2)=n then true else false

let rec fastexpt : int -> int -> int
= fun b n -> match n with
            | 1 -> b
            | 2 -> b*b
            | _ -> if check_even n then fastexpt (fastexpt b (n/2)) 2 else b*(fastexpt b (n-1))