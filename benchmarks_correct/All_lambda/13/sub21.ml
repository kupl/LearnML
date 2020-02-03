type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec isthere s lst =
	match lst with
	|[] -> false
	|hd::tl -> if (hd = s) then true
		   else (isthere s tl)

let rec check2 mtr lst =
	match mtr with
	|V x -> (isthere x lst)
	|P (str,m) -> (check2 m (str::lst))
	|C (m1,m2) -> (check2 m1 lst) && (check2 m2 lst)

let check m =
	check m []


