(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
  match n with
  |0 -> 1
  |_ ->
    if (n mod 2)=1 then b * (fastexpt b (n-1))
    else (fastexpt b (n/2))*(fastexpt b (n/2)) 