(* 
2011-10634
JooHyun Jo / Major in Economics
problem 2 for HW1
*)

let rec sigma f a b =
  if a>b
    then 0
    else f a + sigma f (a+1) b

