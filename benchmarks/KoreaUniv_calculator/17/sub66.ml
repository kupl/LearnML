(*problem 5*)
type exp=
X
|INT of int
|ADD of exp*exp
|SUB of exp*exp
|MUL of exp*exp
|DIV of exp*exp
|SIGMA of exp*exp*exp
let rec insert:int->int->int list->int list=fun a b l->
if b=a then b::l
else insert a (b-1) (b::l)

let rec cal:exp->int->int=fun e n ->
match e with
|INT a-> a
|X-> (fun x->x) n  
|ADD(a,b)-> ((cal a n)-(cal b n))
|SUB(a,b)-> ((cal a n)-(cal b n))
|MUL(a,b)-> ((cal a n)*(cal b n))
|DIV(a,b)-> ((cal a n)/(cal b n))
|SIGMA(a,b,c)-> if (cal a n)=(cal b n) then (cal (c) (cal a n)) else let l1= (cal (c) (cal a n)) in let l2=SIGMA(INT((cal a n)+1),b,c) in let l3=cal l2 n in l1+l3

let calculator:exp->int=fun e->cal (e) 0