type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
(*
let rec crazy2val = function
	| NIL -> 0
	| ZERO NIL -> 0
	| ONE NIL -> 1
	| MONE NIL -> -1
	| ZERO c -> 2*(crazy2val c)
	| ONE c -> 1+2*(crazy2val c)
	| MONE c -> -1+2*(crazy2val c)
*)

let rec crazy2add = function
	(* base *)
	| ( c, NIL ) | ( NIL , c ) -> c
	(* no carry *)
	| ( ZERO c1, ZERO c2 ) | ( ONE c1, MONE c2 ) | ( MONE c1, ONE c2 ) ->ZERO ( crazy2add ( c1, c2 ) )
	| ( ZERO c1, ONE c2 ) | ( ONE c1, ZERO c2 ) -> ONE ( crazy2add  ( c1, c2 ) )
	| ( ZERO c1, MONE c2 ) | ( MONE c1, ZERO c2 ) -> MONE ( crazy2add ( c1, c2 ) )
	(* carry *)
	| ( ONE c1, ONE c2 ) -> ZERO ( crazy2add ( crazy2add ( ONE NIL, c1 ), c2 ) )
	| ( MONE c1, MONE c2 ) -> ZERO ( crazy2add ( crazy2add ( MONE NIL, c1 ) , c2 ) )

(*
let _= let print_bool x = print_endline (string_of_bool x) in
print_bool ( 0=(crazy2val (crazy2add (ZERO NIL, ZERO NIL)) ) );
print_bool ( 0=(crazy2val (crazy2add (MONE NIL, ONE NIL))));
print_bool ( 1=(crazy2val (crazy2add (ZERO NIL, ONE NIL))));
print_bool ( 4=(crazy2val (crazy2add (ONE(ONE NIL), ONE NIL))));
print_bool ( -683 = (crazy2val (crazy2add ( MONE(ZERO(ZERO(ZERO NIL))), (ZERO(ONE(ONE(ZERO(MONE(ONE(ONE(ZERO(ONE(ZERO(MONE NIL)))))))))))))));;
*)
