type nat = 	ZERO
	|SUCC of nat

let rec natadd = fun (n1, n2)->
	match n1 with 
	|ZERO -> n2
	|SUCC m -> natadd(m, SUCC n2)

let rec natmul = fun (n1, n2)-> 
	match n1 with
	|ZERO -> ZERO
	|SUCC m -> natadd(n2, natmul(m,n2))

