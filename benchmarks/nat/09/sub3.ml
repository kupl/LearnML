type nat = ZERO | SUCC of nat;;

let rec natadd a b =
	match a, b with
		| ZERO, ZERO -> ZERO
		| ZERO, b -> b 
		| a, ZERO -> a 
		| SUCC a, SUCC b -> (SUCC (SUCC (natadd a b)))
;;

let rec natmul a b =
    match a, b with
        | ZERO, b   -> ZERO
        | a, ZERO   -> ZERO
        | SUCC ZERO, b -> b
        | a, SUCC ZERO -> a
        | a, SUCC b ->  (natadd a (natmul a b))
;;

let rec natenum a =
	match a with
		| ZERO -> 0
		| SUCC num -> 1 + (natenum num)
;;