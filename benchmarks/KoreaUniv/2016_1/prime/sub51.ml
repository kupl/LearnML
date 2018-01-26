let rec prime n =
  let rec div d = 
	if d = 1 then 1
	else if n mod d = 0 then -1
	else div(d-1) in
if div(n-1) = 1 then true
else false
