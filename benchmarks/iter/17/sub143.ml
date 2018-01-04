
let rec iter ((n:int),(f: int->int)) (x:int) :int =
  if n<=0 then x
  else f (iter(n-1,f) x)

(*
  let a31 = iter (3, function x -> 2+x) 0
  let a32 = iter (0, function x -> 2*x) 4
  let a33 = iter (11, function x -> 2*x+1) 7 *)
