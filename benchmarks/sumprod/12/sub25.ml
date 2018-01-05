(* hw1-4 *)
(* 2010-11687 Keunjun Choi *)

exception ERROR of string
let sumprod (matrix, n, k) = 
	let rec production (i, j) =
		if j=k then matrix (i, j)
		else matrix (i, j)*.production (i, j+1)
	in
	let rec summation i =
		if i=n then production (i, 1)
		else production (i, 1)+.summation (i+1)
	in
	if n<1 || k<1 then raise (ERROR "n>=1 and k>=1")
	else summation 1
