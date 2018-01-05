(* 
2011-10634
JooHyun Jo / Major in Economics
problem 3 for HW1
*)

let rec iter ((n:int), (f:'a->'a)):('a->'a)=
  match n with
  |0 -> fun x -> x
  |_ -> fun x -> (iter ((n-1), f)) (f x) 
  
