(* problem 2*)
type nat = ZERO | SUCC of nat

add : nat -> nat -> nat 
= fun n1 n2 -> [n,p:Nat] [X:*] [f:X->X] [a:X] n X f (p X f a)

let natmul : nat -> nat -> nat 
= fun n1 n2 -> [n,p:Nat] [X:*] [f:X->X] [a:X] n X (p X f) a

let natexp : nat -> nat -> nat 
= fun n1 n2 -> [n,p:Nat] [X:*] p (X -> X) (n X)
