exception Error of string
let rec sigma f a b =
  	if (a > b) then raise (Error ("a is bigger than b"))
  	else if (a = b) then (f a)
  	else (f a) + (sigma f (a+1) b)