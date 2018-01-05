
(* 2008-11720 Á¶°Ü¸® *)

let rec sumprod ( mat, n, k) =

let rec pie n k=
if (k=0) then 1.
else mat(n, k)*.(pie n (k-1))
in

if (n=0) then 0.
	else (pie n k)+.(sumprod (mat, n-1, k))

