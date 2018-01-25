(*problem1*)
let rec pascal(x,y)=
	if y=0 || x=y then 1
	else pascal(x-1,y-1)+pascal(x-1,y)

(*problem2*)
let rec sigma f a b =
	if a>b then 0
	else (f a) + sigma f(a+1) b

(*problem3*)
let rec max l=match l with
        |[]->0
  	|a::[]->a
  	|a::b->
  		let c=max b in
  		if a>c then a
  		else c

let rec min l=match l with
        |[]->0
  	|a::[]->a
  	|a::b->
  		let c=min b in
  		if a<c then a
  		else c


(*problem4*)
type form=
	True
	|False
	|Neg of form
	|Or of form*form
	|And of form*form
	|Imply of form*form
	|Equiv of form*form

let rec eval l=match l with
	|True->true
	|False ->false
	|Neg (l)->if (eval l)=true then false else true
	|Or(l1,l2)->if(eval l1)=true || (eval l2)=true then true else false
	|And(l1,l2)->if(eval l1)=true && (eval l2)=true then true else false
	|Imply(l1,l2)->if(eval l1)=true then true else false
	|Equiv(l1,l2)->if(eval l1)==(eval l2)=true then true else false


(*problem5*)
type nat = ZERO|SUCC of nat

let rec natadd a=function
	ZERO->a
	|SUCC b-> natadd(SUCC a) b
let rec natmul a b =match b with
	ZERO->ZERO
	|SUCC c-> natadd(natmul a c) a



	







