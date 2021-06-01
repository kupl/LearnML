(*
  CSE/2015-21233/김종권
  Homework 1-3
*)
let rec iter' (n, f) =
  if n = 0 then (fun x->x) else
    (fun x -> (iter' (n-1, f)) (f x))
     
let iter (n, f) =
  iter' (n, f) 
