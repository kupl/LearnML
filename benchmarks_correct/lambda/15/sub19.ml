type lambda =
V of string
|P of string*lambda
|C of lambda*lambda;;

let check lambda =
	let rec chk st at =
		match at with
		|hd::tl -> if hd = st then true else (chk st tl)
		|[] -> false
	in
	let rec foo mtr lst = 
		match mtr with
		|V s -> (chk s lst)
		|P (a,rest) -> (foo rest (a::lst))
		|C (a,b) -> (foo a lst)&&(foo b lst)
	in
	(foo lambda [])
;;

