(*
  CSE/2015-21233/김종권
  Homework 1-2
*)
let rec sigma' (a, b, f) =
  if b < a then 0
  else (f a) + (sigma' (a+1, b, f))
let rec sigma (a, b, f) =
  sigma' (a, b, f)
