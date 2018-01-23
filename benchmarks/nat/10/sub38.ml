type nat = ZERO
	 | SUCC of nat ;;



let rec natadd (n1, n2) =
	match n1 with
	SUCC k -> SUCC (natadd (k,n2)) 
	| ZERO -> n2 ;;

let rec natmul (n1, n2) =
	match n1 with
	SUCC k -> (natadd (n2, (natmul (k, n2))))
	| ZERO -> ZERO
