let rec prime n = let rec dividing d =
if n mod d = 0 then false
else if d*d > n then true  else dividing (d+1) in
if n<=0 then raise (Failure "the number should be positive")
else if n=1 then false
else if n=2 then true 
else dividing 2;; 
