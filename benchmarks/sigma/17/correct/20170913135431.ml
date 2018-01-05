(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-7
  Homework-# : 1-2
  Excercise-Name : Sigma
*)

let rec sigma (a,b,f) =
  match (a,b) with
  | (a,b) when a > b -> 0
  | (a,b) when a = b -> f a
  | (a,b) -> (
    (f a) + sigma (a+1, b, f)
  )

