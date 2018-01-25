(* problem 1*)

let rec fastexpt : int -> int -> int = fun b n ->
  match n with
  | 0 -> 1
  | _ -> if n mod 2 = 0
    then let n2 = fastexpt b (n / 2) in n2*n2
    else b * fastexpt b (n - 1);;