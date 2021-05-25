(*problem 2*)
type nat = ZERO | SUCC of nat

let rec inttonat n =
match n with
|0->ZERO
|_-> SUCC(inttonat(n-1))

let rec nattoint n =
match n with
|ZERO -> 0
|SUCC(n1) -> 1+(nattoint n1)


let rec  natadd : nat -> nat -> nat
= fun n1 n2 ->
let a = nattoint n1 in
let b = nattoint n2 in
let c = (a+b) in (inttonat c)

let rec  natmul :nat->nat->nat
= fun n1 n2->
let a = nattoint n1 in
let b = nattoint n2 in
let c= (b*a) in (inttonat c)


let rec exp a b =
if b=0 then 1
else a*exp a (b-1)

let rec  netexp : nat -> nat -> nat
= fun n1 n2 ->
let a = nattoint n1 in
let b= nattoint n2 in
let c= exp a b in (inttonat c)