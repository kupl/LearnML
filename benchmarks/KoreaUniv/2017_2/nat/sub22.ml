(* problem 2*)
  type nat = ZERO | SUCC of nat;;

  let rec natadd : nat -> nat -> nat
  = fun a b -> match a with
  | ZERO -> b 
  | SUCC n -> natadd n (SUCC(b));;

  let natmul : nat -> nat -> nat 
  = fun a b -> let c=b in
  let rec natmulrec = fun t s u -> match t with
  | ZERO -> ZERO
  | SUCC n -> match n with
    | ZERO -> s
    | SUCC n -> natmulrec (SUCC(n)) (natadd s c) c in
    natmulrec a b c;;

  let natexp : nat -> nat -> nat
  = fun a b -> let c=a in
  let rec natexprec = fun t s u -> match s with
  | ZERO -> SUCC ZERO
  | SUCC n -> match n with
    | ZERO -> t 
    | SUCC n -> natexprec (natmul t u) (SUCC n) u in
    natexprec a b c;;
