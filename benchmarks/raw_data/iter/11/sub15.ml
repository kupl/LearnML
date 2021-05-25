(* PL HW1-2 "반복기"
   2007-11738
   알렉산더 *)

(* iter: int * (int -> int) -> int -> int *)
let rec iter (n, f) x =
    if n = 0 then x
    else f (iter (n-1, f) x)
