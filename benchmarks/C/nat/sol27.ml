type nat = ZERO | SUCC of nat  
  
let rec natLength : nat -> nat -> int
 =fun n1 n2 ->
  if n1 = n2 then 0
  else (natLength n1 (SUCC(n2))) + 1
  
let rec makeNat : int -> nat
=fun n ->
  if n = 0 then ZERO
  else SUCC(makeNat(n-1))

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> (* TODO *)
  makeNat((natLength n1 ZERO) + (natLength n2 ZERO))  

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> (* TODO *)
  makeNat((natLength n1 ZERO) * (natLength n2 ZERO))
