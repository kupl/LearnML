type nat =
	| ZERO
	| SUCC of nat

let rec make n =
match n with
| 0 -> ZERO
| _ -> SUCC( make (n-1))

let rec natcnt n =
match n with
| ZERO -> 0
| SUCC n1 -> 1 +( natcnt n1)

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
let v1 = natcnt n1 in
let v2 = natcnt n2 in
let v3 = (v2 + v1) in (make v3)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
let v1 = natcnt n1 in
let v2 = natcnt n2 in
let v3 = (v2 * v1) in (make v3)
