(*problem 2*)
type nat = ZERO | SUCC of nat
let two = SUCC (SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

    let rec natadd
    =fun n1 n2 -> match n2 with
    |ZERO -> let rec subnat n = match n with
    |ZERO -> ZERO
    |SUCC x ->
    SUCC(subnat x)
  in
  subnat n1
    |SUCC y -> SUCC (natadd n1 y);;

    let rec natmul
    = fun n1 n2 -> match n2 with
    |ZERO -> ZERO
    |SUCC n2 -> natadd n1 (natmul n1 n2);;