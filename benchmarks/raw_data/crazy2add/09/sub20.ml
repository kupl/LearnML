(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Exercise7.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 7*)
exception Error of string 
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2 ;;
let rec crazy2add (cra, crb)=

	let rec fone x= 
	match x with
	| NIL -> ONE NIL
	| ZERO p -> ONE p
	| ONE  p -> ZERO (fone p)
	| MONE p -> ZERO p                 
	in
	let rec fmone x= 
	match x with
	|NIL -> MONE NIL
	|ZERO p -> MONE p
	|ONE p -> ZERO p
	|MONE p -> ZERO (fmone p)       
 	in
	let rec fzero x=
	match x with
	|ZERO (ZERO NIL)  -> ZERO NIL 
	|ZERO x -> ZERO (fzero x)
	| _ -> x	
	in
	let rec crazy2addsub (a, b)=
	match (a, b) with
  	|(NIL,NIL) -> NIL
  	|(x, NIL) -> x
  	|(NIL, y) -> y
  	|(ZERO x, ZERO y) -> (fzero (ZERO (crazy2addsub(x,y))))
  	|(ZERO x, ONE y) ->  (fzero (ONE (crazy2addsub(x,y))))
  	|(ZERO x, MONE y) ->  ( fzero (MONE (crazy2addsub(x,y))))
  	|(ONE x, ZERO y) -> (fzero (ONE (crazy2addsub(x,y))))
  	|(ONE x, ONE y) -> (fzero (ZERO (crazy2addsub((fone x),y))))
  	|(ONE x, MONE y) -> (fzero ( ZERO (crazy2addsub(x,y))))
  	|(MONE x, ZERO y) -> (fzero ( MONE (crazy2addsub(x,y))))
  	|(MONE x, ONE y) -> (fzero ( ZERO (crazy2addsub(x,y))))
  	|(MONE x, MONE y) -> (fzero ( ZERO (crazy2addsub((fmone x),y))))
  	in
	if cra=NIL && crb=NIL then raise ( Error "Illegal input")
	else
	crazy2addsub (cra,crb)
  ;;

