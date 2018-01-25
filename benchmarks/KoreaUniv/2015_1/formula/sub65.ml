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
