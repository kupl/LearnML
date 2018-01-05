(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-13
  Homework-# : 2-2
  Excercise-Name : k-nary number(k-chin-su)
*)

type crazy2 = NIL 
| ZERO of crazy2 
| ONE of crazy2 
| MONE of crazy2

let crazy2val crazy2 = (
  let rec crazy2sum crazy2 exp = 
    match crazy2 with
    | NIL -> 0
    | ZERO cr2 -> 0 * (Int.pow 2 exp) + (crazy2sum cr2 (exp+1))
    | ONE cr2 -> 1 * (Int.pow 2 exp) + (crazy2sum cr2 (exp+1))
    | MONE cr2 -> -1 * (Int.pow 2 exp) + (crazy2sum cr2 (exp+1))
  in crazy2sum crazy2 0
)
