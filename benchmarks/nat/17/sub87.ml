type nat = ZERO | SUCC of nat
let rec natadd : nat * nat -> nat = fun (a, b) ->
  match a with
  |ZERO -> b
  |SUCC ia -> natadd (ia, SUCC b)

let rec natmul : nat * nat -> nat = fun (a, b) ->
  match b with
  |ZERO -> ZERO
  |SUCC ib -> natadd(a, natmul(a, ib))
(*
let nat0 = ZERO
let nat1 = SUCC nat0
let nat2 = SUCC nat1
let nat3 = SUCC nat2
let nat4 = SUCC nat3

let rec nat_to_int nat =
  match nat with
  |ZERO -> 0
  |SUCC i -> (1+(nat_to_int i))

let print_nat nat = print_int (nat_to_int nat)

let _ = print_string("0+2 : "); print_nat(natadd(nat0, nat2)); print_newline()
let _ = print_string("1+1 : "); print_nat(natadd(nat1, nat1)); print_newline()
let _ = print_string("3+2 : "); print_nat(natadd(nat3, nat2)); print_newline()
let _ = print_string("2*3 : "); print_nat(natmul(nat2, nat3)); print_newline()
let _ = print_string("1*0 : "); print_nat(natmul(nat1, nat0)); print_newline()
let _ = print_string("0*2 : "); print_nat(natmul(nat0, nat2)); print_newline()
let _ = print_string("0*0 : "); print_nat(natmul(nat0, nat0)); print_newline()


let three = SUCC(SUCC(SUCC ZERO))
let four = SUCC three

let _ = print_nat (natadd (three, four));
print_nat (natadd (ZERO, ZERO));
print_nat (natadd (ZERO, three));
print_nat (natadd (four, ZERO));*)
(*
  print_bool (12 = nat_to_int (natmul (three, four)));
  print_bool (0 = nat_to_int (natmul (ZERO, three)));
  print_bool (0 = nat_to_int (natmul (four, ZERO)));
  print_bool (0 = nat_to_int (natmul (ZERO, ZERO)));
  print_bool (3 = nat_to_int (natmul (SUCC ZERO, three)));
  print_bool (4 = nat_to_int (natmul (four, SUCC ZERO)));*)
