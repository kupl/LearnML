type nat = ZERO | SUCC of nat

let rec natadd ((a:nat),(b:nat)):nat = 
	match a with 
	ZERO -> b
	|SUCC something-> natadd (something,SUCC b)  
;;

let rec natmul ((a:nat),(b:nat)):nat = 
	match a with
	ZERO->ZERO
	|SUCC something-> natadd (b,(natmul (something,b)))
;;
