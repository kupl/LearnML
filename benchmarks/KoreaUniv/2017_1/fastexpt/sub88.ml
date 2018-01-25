(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> match n with
| 0 -> 1
| 1 -> b
| 2 -> b*b
| _ -> if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2))
  else b * (fastexpt b (n-1));;
