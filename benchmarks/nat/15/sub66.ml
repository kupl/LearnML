type nat = ZERO | SUCC of nat

let rec natadd (ln: nat) (rn: nat): nat = 
	match ln with
	| ZERO -> rn
	| SUCC snat -> (natadd snat (SUCC rn))
	
let rec mult ln rn sum: nat = 
	match ln with
	| ZERO -> sum
	| SUCC snat -> (mult snat rn (natadd rn sum))
	
let rec natmul (ln: nat) (rn: nat): nat = 
	match ln with
	| ZERO -> ZERO
	| SUCC _ -> (mult ln rn ZERO)
