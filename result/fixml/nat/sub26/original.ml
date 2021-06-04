type nat = ZERO | SUCC of nat

let natdec : nat -> nat = fun n -> match n with SUCC n' -> n' | ZERO -> ZERO

let rec natadd : nat -> nat -> nat =
 fun n1 n2 -> if n1 = ZERO then n2 else natadd (natdec n1) (SUCC n2)


let rec natmul : nat -> nat -> nat =
 fun n1 n2 -> if n1 = SUCC ZERO then n2 else natadd n2 (natmul (natdec n1) n2)


let two = SUCC (SUCC ZERO)

let three = SUCC (SUCC (SUCC ZERO))

let _ = natadd two three

let _ = natmul two three
