type nat = ZERO | SUCC of nat 

let rec natadd nat1 nat2 = 
	match nat1 with
	ZERO -> nat2
	| SUCC(na) -> natadd na (SUCC(nat2)) ;;

 let rec natmul nat1 nat2 = 
 	match nat1 with
	ZERO -> ZERO
	| SUCC(ZERO) -> nat2
	| SUCC(na) -> if nat2 = ZERO then ZERO 
		      else if nat2 = SUCC(ZERO) then nat1 
		      else natadd (natmul na nat2) nat2 ;;

