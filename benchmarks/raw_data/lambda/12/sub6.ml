
type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string


let check tak =
  	let rec makeList a lst = 
  		match a with
  			V s -> List.mem s lst
  			|P(x,y) -> makeList y (x::lst)
  			|C(x,y) -> (makeList x lst) && (makeList y lst)
  	in
  	makeList tak []