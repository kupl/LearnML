(* PL HW1-1 "씨그마"
   2007-11738
   알렉산더 *)

(* sigma: int * int * (int->int) -> int *)
let rec sigma func a b =
        if a>b then 0
        else (func a) + (sigma func (a+1) b)
