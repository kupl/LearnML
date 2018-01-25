(* problem 1*)
let square x = x * x

let rec fastexpt : int -> int -> int
= fun b n ->
match n with
  0 -> 1
| 1 -> b
| _ -> match n mod 2 with 
      | 0 -> square(fastexpt b (n/2))
      | _ -> square(fastexpt b (n/2)) * b