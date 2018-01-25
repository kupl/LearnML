(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) ->
  if x=y then 1
  else match (x,y) with
  (_,0) -> 1
  | _ ->let rec facto a=       
  if a=1 then 1 else a*facto(a-1) in
  (facto x)/((facto y)*(facto (x-y)));;

(* version that does not use recursive definition but uses factorial function and definition formula of pascal triangle *)
