type nat = ZERO | SUCC of nat

let x = fun nat -> SUCC(nat);;

let rec z : nat -> int -> nat
= fun a b ->
  if (b = 0) then a
  else 
    let c = x (a) in
    z c (b-1);;

let rec f : nat -> nat -> int -> int
= fun a b c ->
  if (a = b) then c 
  else 
    let d = x b in
    f a d (c+1);;

let natadd : nat -> nat -> nat
= fun n1 n2 -> let a = f n1 ZERO 0 and b = f n2 ZERO 0 in
let c = a + b in
 z ZERO c;;
 
let natmul : nat -> nat -> nat
= fun n1 n2 -> let a = f n1 ZERO 0 and b = f n2 ZERO 0 in
let c = a * b in
 z ZERO c;;
 
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natadd two three;;
natmul two three;;
