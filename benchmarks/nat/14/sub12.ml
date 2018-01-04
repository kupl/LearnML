type nat = ZERO | SUCC of nat ;;

let rec nat_to_num origin =
	match origin with
	| ZERO -> 0
	| SUCC param -> 1 + nat_to_num param
;;
let rec num_to_nat origin =
	if origin=0 then ZERO
	else SUCC ( num_to_nat ( origin - 1 ) )
;;

let natadd (first, second) =
	num_to_nat ( nat_to_num first + nat_to_num second )
;;
let natmul (first, second) =
	num_to_nat ( nat_to_num first * nat_to_num second )
;;
