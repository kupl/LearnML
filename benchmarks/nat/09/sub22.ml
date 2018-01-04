type nat = ZERO | SUCC of nat

let rec natadd(x,y:nat*nat) = 
  	match x with
  		SUCC(z) -> natadd(z,SUCC(y))
  		| ZERO -> y

let natmul(x,y:nat*nat) = 
  	let rec natadd(a,b:nat*nat) = 
  		match a with
	  		SUCC(c) -> natadd(c,SUCC(b))
  			| ZERO -> b
  	in 
  	let result = ref ZERO in
  	let rec loop(index) = 
  		match index with
  			SUCC(z) -> (result := natadd(y,!result);(loop(z)))
  			| ZERO -> !result
  	in
  	loop(x);;