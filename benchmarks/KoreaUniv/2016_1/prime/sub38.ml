let rec  prime n
= let rec prime_check n d=
	if (n<=0) then failwith "error : prime's element (n<=0)"
	else if (n=2) then true
	else if (n<2) then false
	else if (n=d) then true
	else if ((n mod d)=0) then false
	else prime_check n (d+1)
in if prime_check n 2 then true else false;;
