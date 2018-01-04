type nat = ZERO | SUCC of nat;;

  
let rec natadd nats =
  	match nats with
	| (ZERO, ZERO)-> ZERO
  	| (ZERO, SUCC a) -> SUCC a
  	| (SUCC a, ZERO) -> SUCC a
  	| (SUCC a, SUCC b)-> natadd (a, SUCC(SUCC b));;

let rec natmul nats = 

	let rec natadd nats1 =
  	match nats1 with
	| (ZERO, ZERO)-> ZERO
  	| (ZERO, SUCC a) -> SUCC a
  	| (SUCC a, ZERO) -> SUCC a
  	| (SUCC a, SUCC b)-> natadd (a, SUCC(SUCC b))
	in
	match nats with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC a, SUCC b) -> natadd(SUCC b, (natmul (a, SUCC b)));;