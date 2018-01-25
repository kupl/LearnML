(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
    if n == 0 then 1
    else match n mod 2 with
    | 0 -> fastexpt (b * b) (n / 2)
    | _ -> b * (fastexpt b (n - 1))