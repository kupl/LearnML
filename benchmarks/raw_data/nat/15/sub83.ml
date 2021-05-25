type nat = ZERO | SUCC of nat;;

let rec findnat x= if (x==0) then ZERO else SUCC(findnat (x-1));;
let rec natvalue n= match n with |ZERO -> 0 | SUCC(t) -> 1 +  (natvalue t);;
let rec down x= if (x==ZERO) then ZERO else findnat ((natvalue x)-1);; 
let rec natadd n1 n2 = if ((natvalue n1)==0) then n2 else (natadd (down n1) (SUCC(n2)) );;
let rec natmul n1 n2= if ( n2==ZERO ) then ZERO else (natadd n1 (natmul (down(n2)) n1) );; 



