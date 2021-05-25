type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> let rec f a =
              match a with
              | ZERO -> ZERO
              | SUCC (b) -> SUCC (f b)
            in f n2
  | SUCC (c) -> SUCC (natadd c n2)  

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC (c) -> (natadd n2 (natmul n2 c))
