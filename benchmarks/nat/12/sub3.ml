type nat =
	ZERO 
	| SUCC of nat

let rec nat2int nat =
	match nat with
	| ZERO -> 0
	| SUCC nat -> 1 + nat2int nat

let rec int2nat i =  
	if i=0 then ZERO
	else SUCC( int2nat (i-1) )
	

let rec natadd( nat1, nat2 ) = 
	match( nat1, nat2 ) with 
	| (nat1, nat2 ) -> int2nat( nat2int nat1 + nat2int nat2 )

let natmul( nat1, nat2 ) =
	match( nat1, nat2 ) with 
	| (nat1, nat2 ) -> int2nat( nat2int nat1 * nat2int nat2 )