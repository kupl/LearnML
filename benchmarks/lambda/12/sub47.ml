type lambda =
V of string
|P of string*lambda
|C of lambda*lambda;;

let check lambda =
	let rec chk what at =
		match at with
		|hd::tl -> if hd = what then true else (chk what tl)
		|[] -> false
	in
	let rec foo mtr lst = 
		match mtr with
		|V n -> (chk n lst)
		|P (n,rest) -> (foo rest (n::lst))
		|C (a,b) -> (foo a lst)&&(foo b lst)
	in
	(foo lambda [])
;;

