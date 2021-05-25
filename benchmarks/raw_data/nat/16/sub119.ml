type nat=
|ZERO
|SUCC of nat
let rec natadd:nat->nat->nat
=fun n1 n2->match n2 with
|ZERO -> n1
|SUCC m-> SUCC(natadd n1 m)

let rec natmul:nat->nat->nat
=fun n1 n2->match n2 with
|ZERO->ZERO
|SUCC m->natadd n1 (natmul n1 m);;
 