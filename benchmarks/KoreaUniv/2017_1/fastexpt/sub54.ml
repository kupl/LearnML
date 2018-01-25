(* problem 1*)
let rec fastexpt: int -> int -> int 
= fun b n -> if n = 1 then b 
    else match n mod 2 with
         0 -> fastexpt b (n/2) * fastexpt b (n/2)
        |_ -> b * (fastexpt b (n-1));;