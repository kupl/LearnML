(*problem 2*)

type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
ZERO -> n1
|SUCC( k) -> natadd (SUCC n1) k;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
let n3 = n1 in
let rec hold1 = fun n1 n2 n3 ->
match n2 with
ZERO -> ZERO
|SUCC ZERO -> n1
|SUCC (k) -> hold1 (natadd n1 n3) k n3 in hold1 n1 n2 n3;;

let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
let n3 = n1 in
let rec hold2 = fun n1 n2 n3->
match n2 with
ZERO -> SUCC ZERO
|SUCC ZERO -> n1
|SUCC (k) -> hold2 (natmul n1 n3) k n3 in hold2 n1 n2 n3;;
