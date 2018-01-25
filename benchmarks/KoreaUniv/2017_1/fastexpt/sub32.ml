(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
        if n = 0 then 1
        else if n mod 2 = 0 then let x = fastexpt b (n/2) in x*x
        else b * (fastexpt b (n-1))
