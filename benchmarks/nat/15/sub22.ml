type nat = ZERO |SUCC of nat

let rec natadd(n1,n2)=
	match(n1,n2) with
	|_,SUCC n2' -> natadd((SUCC n1),n2')
	|_,ZERO -> n1
;;

let rec natmul(n1,n2)=
	match(n1,n2) with
	|ZERO,_ -> ZERO
	|_,ZERO -> ZERO
	|_,SUCC n2' -> natadd(n1,natmul(n1,n2'))
;;
