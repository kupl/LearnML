type nat = ZERO | SUCC of nat 

let rec natadd (n1, n2) =
	match n1 with
	| ZERO -> 
		begin
		match n2 with
		|ZERO -> ZERO
		|SUCC (n2') -> n2
		end
	| SUCC(n1') ->
		begin
		match n2 with
		|ZERO -> n1
		|SUCC (n2') -> natadd(n1', (SUCC n2))
		end

let natmul (n1, n2) =
	match n1 with
	|ZERO -> ZERO
	|SUCC (n1') ->
		let rec countnat n11 =
			match n11 with
			|ZERO -> 0
			|SUCC (n11') -> 1+ countnat(n11')
		in

		let rec retnat n =
			match n with
			|0 -> ZERO
			|n' -> SUCC (retnat (n-1))
		in
		
		let intmul (i1, i2) = i1 * i2
		in

		begin
		match n2 with
		|ZERO -> ZERO
		|SUCC(n2') -> retnat(intmul(countnat n1, countnat n2))
		end

