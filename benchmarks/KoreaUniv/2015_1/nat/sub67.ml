type nat = ZERO | SUCC of nat;;

let rec natadd nat1 nat2 =
	match nat1 with
		ZERO->nat2
		|SUCC natt -> SUCC (natadd natt nat2);;


let rec natmul nat1 nat2 =
		match nat1 with
			ZERO -> ZERO
			|SUCC nat3 -> natadd nat2 (natmul nat3 nat2) ;; 


