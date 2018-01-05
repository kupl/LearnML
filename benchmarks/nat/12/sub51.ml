type nat = 
	 ZERO
	|SUCC of nat

let rec natadd ((num1:nat), (num2:nat)) =
	match num1 with
	|SUCC a -> SUCC (natadd (a, num2))
	|ZERO -> num2

let rec natmul ((num1:nat), (num2:nat)) = 
	match num1 with
	|SUCC a -> (natadd ((natmul (a, num2)), num2))
	|ZERO -> ZERO
