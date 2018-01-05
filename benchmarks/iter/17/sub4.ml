(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-7
  Homework-# : 1-3
  Excercise-Name : Iterator
*)

let rec iter ((n:int),(f:'a->'a)) arg = 
  match n with
  | n when n <=0 -> arg
  | n -> (
    let arg' = f arg in
    iter(n-1, f) arg'
  ) 
;;

