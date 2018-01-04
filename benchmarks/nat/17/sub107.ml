type nat= ZERO | SUCC of nat

let rec natadd ((n1:nat), (n2:nat)) : nat =
  match n2 with
  | ZERO -> n1
  | SUCC n22 -> SUCC (natadd (n1, n22))

let rec natmul ((n1:nat), (n2:nat)) : nat =
  match n2 with
  | ZERO -> ZERO
  | SUCC n22 -> (natadd ((natmul (n1, n22)), n1))
(*
let rec nat_to_int : nat -> int =
  fun n ->
    match n with
      | ZERO -> 0
      | SUCC n1 -> 1 + (nat_to_int n1)

let print_bool x =
  print_endline (string_of_bool x)

let three = SUCC (SUCC (SUCC ZERO))

let four = SUCC three
let _= print_int(nat_to_int(three))

let _ = print_int(nat_to_int(natadd(three,four)));
*)

(*
let three = SUCC (SUCC (SUCC ZERO))
let four= (SUCC three)
let _ = 
  if (natadd(three,(SUCC ZERO))= four)
  then print_endline("true")
  else print_endline("false")
*)

(*
print_int(nat_to_int(three));
print_bool (7 = nat_to_int (natadd (three, four)));
print_bool (0 = nat_to_int (natadd (ZERO, ZERO)));
print_bool (3 = nat_to_int (natadd (ZERO, three)));
print_bool (4 = nat_to_int (natadd (four, ZERO)));

print_bool (12 = nat_to_int (natmul (three, four)));
print_bool (0 = nat_to_int (natmul (ZERO, three)));
print_bool (0 = nat_to_int (natmul (four, ZERO)));
print_bool (0 = nat_to_int (natmul (ZERO, ZERO)));
print_bool (3 = nat_to_int (natmul (SUCC ZERO, three)));
print_bool (4 = nat_to_int (natmul (four, SUCC ZERO))) 
*)

(*  match (n1, n2) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, SUCC of n22) -> n2
  | (SUCC of n11, ZERO) -> n1
  | (SUCC of n11, SUCC of n22) -> (SUCC of (natadd n11, n2))
*)

