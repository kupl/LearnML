(* ex 4 *)
type nat = ZERO | SUCC of nat
let rec natadd : nat * nat -> nat = fun (a, b) ->
  match (b) with
    |ZERO -> a
    |SUCC bt -> natadd(SUCC a, bt)

let natmul : nat * nat -> nat = fun (a, b) ->
  let rec natmul_rec : nat * nat * nat -> nat = fun (a, b, r) ->
    match (b) with
      |ZERO -> r
      |SUCC bt -> natmul_rec(a, bt, natadd(a, r))
  in natmul_rec(a, b, ZERO)

(* test *)
(*
let rec make_nat : int -> nat = fun (x) ->
  if (x > 0) then SUCC (make_nat(x-1))
  else ZERO

let rec make_int : nat -> int = fun (x) ->
  match (x) with
    |ZERO -> 0
    |SUCC xt -> make_int(xt) + 1

let n3 = make_nat(3)
let n5 = make_nat(5)
let n8 = natadd(n3, n5)
let n15 = natmul(n3, n5)
let n120 = natmul(n15, n8)
let i8 = make_int(n8)
let i15 = make_int(n15)
let i120 = make_int(n120)
let _ = print_int i8
let _ = print_int i15
let _ = print_int i120
*)
