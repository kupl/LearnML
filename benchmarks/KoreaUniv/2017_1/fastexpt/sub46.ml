(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b c -> if c = 1 then b
              else if c mod 2 = 1 then b * (fastexpt b (c-1))
              else (fastexpt b (c/2)) * (fastexpt b (c/2))