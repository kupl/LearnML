exception Error of string;;
type nat = ZERO | SUCC of nat;;
let rec natadd ( (a : nat ), ( b : nat ) ) = 
	match a with
		ZERO -> b |
		SUCC c -> SUCC ( natadd (c,b) );;

let rec natmul ( ( a : nat ), ( b : nat ) ) = 
	match a with
		ZERO -> ZERO |
		SUCC c -> natadd ( b, natmul( c, b ) );;
(*
let checker(  ( a : nat ), ( b : nat ) ) =
	let rec inner_checker (  ( a : nat ), ( b : nat ) ) = 
		match ( a, b ) with
			( ZERO, ZERO ) -> true |
			( SUCC a1, SUCC b1 ) -> ( inner_checker( a1, b1 ) ) |
			( _, _ ) -> false in
	if( ( inner_checker( a, b ) ) = true ) then
		print_string "correct!\n"
	else
		print_string "wrong!\n";;

let rec makeNat( a ) =
	match a with 
	0 -> ZERO |
	_ -> SUCC( makeNat( a - 1 ) );;

checker( natadd( makeNat( 152 ), makeNat( 48 ) ), makeNat( 200 ) );;
checker( natmul( makeNat( 152 ), makeNat( 48 ) ), makeNat( 7296 ) );;
checker( natmul( makeNat( 1 ), makeNat( 0 ) ), makeNat( 0 ) );;
checker( natmul( makeNat( 1 ), makeNat( 1 ) ), makeNat( 1 ) );;
checker( natmul( makeNat( 2 ), makeNat( 1 ) ), makeNat( 2 ) );;
checker( natmul( makeNat( 2 ), makeNat( 2 ) ), makeNat( 4 ) );;
checker( natadd( makeNat( 1 ), makeNat( 2 ) ), makeNat( 3 ) );;
*)