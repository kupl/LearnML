(*자유전공학부 2013-13444 박하영*)

type nat = ZERO
		   | SUCC of nat

let rec natadd((a:nat),(b:nat)) : nat = 
	 match a with
		  |ZERO -> b
		  |SUCC n -> SUCC(natadd(n,b)) 

let rec natmul((a:nat),(b:nat)) : nat =
	 match a with
		  |ZERO -> ZERO
		  |SUCC n -> (match b with
						  |ZERO -> ZERO
						  |_ -> natadd(b, natmul(n,b)) )

