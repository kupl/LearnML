(*
  CSE 2012-11226 Kwak Jin Han
	exercise 4
*)


(* "0" / "n->n+1" *)

type nat = ZERO | SUCC of nat

(*
ZERO
SUCC(ZERO)
SUCC(SUCC(ZERO)
*)

let rec natadd (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, b) -> b
	| (a, ZERO) -> a
	| (SUCC a, b) -> natadd (a, SUCC b)

(* let result = natadd (SUCC(ZERO), ZERO) *)

let rec natmul (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, b) -> ZERO
	| (a, ZERO) -> ZERO
	| (a, SUCC b) -> natadd (a, natmul (a, b)) 

(* let resultmul = natmul (SUCC(SUCC(ZERO)), SUCC(SUCC(SUCC(ZERO)))) *)

(*
let _ = 
let rec nat_to_int : nat -> int = 
fun n -> 
match n with 
| ZERO -> 0 
| SUCC n1 -> 1 + nat_to_int n1 
in 

let print_bool x = 
print_endline (string_of_bool x) 
	in 

	let three = SUCC (SUCC (SUCC ZERO)) 
	in 
	let four = SUCC three 
	in 

	print_bool (7 = nat_to_int (natadd (three, four))); 
	print_bool (0 = nat_to_int (natadd (ZERO, ZERO))); 
	print_bool (3 = nat_to_int (natadd (ZERO, three))); 
	print_bool (4 = nat_to_int (natadd (four, ZERO))); 

	print_bool (12 = nat_to_int (natmul (three, four))); 
	print_bool (0 = nat_to_int (natmul (ZERO, three))); 
	print_bool (0 = nat_to_int (natmul (four, ZERO))); 
	print_bool (0 = nat_to_int (natmul (ZERO, ZERO))); 
	print_bool (3 = nat_to_int (natmul (SUCC ZERO, three))); 
	print_bool (4 = nat_to_int (natmul (four, SUCC ZERO))); 
*)

(*
 let _ =
 let rec int_of_nat' (n: nat) (accum: int): int =
 match n with
 | ZERO -> accum
 | SUCC n' -> int_of_nat' n' (accum+1)
 in
 let string_of_nat (n: nat): string =
 int_of_nat' n 0 |> string_of_int
 in
 let rec nat_of_int (n: int): nat =
 match n with
 | 0 -> ZERO
 | n -> SUCC (nat_of_int (n-1))
 in
 let assert_equal (expected: nat) (actual: nat) =
 if expected = actual then print_endline "true"
 else
 let expected_str = string_of_nat expected in
 let actual_str = string_of_nat actual in
 Printf.printf "Expected %s but actual %s\n" expected_str actual_str
															in
																let test_natadd (a: int) (b: int) (expected: int) =
																							let natA = nat_of_int a in
																										 let natB = nat_of_int b in
																												let natExptected = nat_of_int expected in
																													natadd (natA, natB) |> assert_equal natExptected
																													 in
																													 let test_natmul (a: int) (b: int) (expected: int) =
																													 let natA = nat_of_int a in
																													 let natB = nat_of_int b in
																													 let natExptected = nat_of_int expected in
																													 natmul (natA, natB) |> assert_equal natExptected
																													 in
																													 test_natadd 1 3 4;
																													 test_natadd 0 1 1;
																													 test_natadd 10 0 10;
																													 test_natadd 0 0 0;
																													 test_natadd 14 3 17;
																													 test_natmul 1 3 3;
																													 test_natmul 0 3 0;
																													 test_natmul 7 0 0;
																													 test_natmul 4 9 36;
																													 test_natmul 0 0 0;
																													 *)
