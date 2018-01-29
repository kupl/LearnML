(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec list_length : bin->int = fun b1->
	match b1 with
		|[]->0 
		|hd::tl->(list_length tl)+1
;;

let rec bin_to_dec : bin->int = fun b1 ->
let leng = list_length b1 in
	match b1 with
		|[]->0
		|hd::tl->if hd = ONE then (expo 2 (leng-1)) + bin_to_dec tl
				else bin_to_dec tl
;;

let rec dec_to_bin : int->bin = fun n->
if (n/2 != 1 && n/2 !=0 && n mod 2 = 0) then dec_to_bin(n/2)@[ZERO]
else if (n/2 != 1 && n/2 !=0 && n mod 2 = 1) then dec_to_bin(n/2)@[ONE]
else if (n/2 = 1 && n mod 2 = 0) then [ONE;ZERO]
else if (n/2 = 1 && n mod 2 = 1) then [ONE;ONE]
else if n = 1 then [ONE]
else [ZERO]
;;

let rec bmul : bin->bin->bin = fun b1 b2 ->
let a = bin_to_dec b1 in
	let b = bin_to_dec b2 in
		dec_to_bin(a*b)
;;