type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec isthere s lst =
	match lst with
	|[] -> false
	|hd::tl -> if (hd = s) then true
		   else (isthere s tl)

let rec check mtr lst =
	match mtr with
	|V x -> (isthere x lst)
	|P (str,m) -> (check m (str::lst))
	|C (m1,m2) -> (check m1 lst) && (check m2 lst)

let check m =
	check m []


