(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
    if n = 0 then 1
    else
      let b2 = fastexpt b (n / 2) in
        if (n mod 2) = 0 then b2 * b2
        else b * b2 * b2;;