(* 
2011-10634
JooHyun Jo / Major in Economics
problem 1 for HW1
*)

let rec merge ((l1:int list), (l2:int list)) :int list =
  match l1 with
  |[] -> l2
  |h1::t1 ->(
    match l2 with
    |[] -> l1
    |h2::t2 -> (
      if h1<h2 then h2::merge (l1,t2)
      else h1::merge (t1,l2)
    )
  )
