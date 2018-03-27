(* problem 2*)
type nat = ZERO | SUCC of nat;;

let rec number:nat->int
=fun n->
match n with
|ZERO->0
|SUCC(x)->1+(number x);;

let rec operation:int->nat
=fun x->
match x with
|0->ZERO
|_->SUCC(operation (x-1));;

let rec fastexpt:int->int->int
=fun b n->
if n=0 then 1
else if n mod 2=0 then (fastexpt (b) (n/2))*(fastexpt (b) (n/2))
else b*(fastexpt (b) (n-1));;

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
operation((number n1)+(number n2));;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
operation((number n1)*(number n2));;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
operation(fastexpt (number n1) (number n2));;
