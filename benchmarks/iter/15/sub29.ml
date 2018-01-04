(* 생명과학부/ 2011-10915 / 신지민 Homework 1-3 *)

type nat = ZERO | SUCC of nat

let rec inverter : nat -> int = fun nat ->
	match nat with
	| ZERO -> 0
	| SUCC n -> begin
		let i = inverter n in
		i+1
		      end
(*
let a  = SUCC(SUCC ZERO)
let b = inverter a
let _= print_endline(string_of_int b)
*)

let rec recover : int -> nat = fun a ->
	if(a==0) then ZERO
	else SUCC(recover (a-1))

(*
let c = 100
let d = recover c
let e = inverter d
let _= print_endline(string_of_int e)
*)

let natadd : nat * nat -> nat = fun (a,b) ->
	begin
	let i1 = inverter a in
	let i2 = inverter b in
	let result = i1+i2 in	
	recover result
	end
(*
let a1 = natadd (SUCC(SUCC(SUCC(SUCC(ZERO)))), SUCC(ZERO))
let a2 = inverter a1
let _= print_endline(string_of_int a2)
*)

let natmul : nat * nat -> nat = fun (a,b) ->
	begin
	let i1 = inverter a in
	let i2 = inverter b in
	let result = i1*i2 in
	recover result
	end
(*
let b1 = natmul (SUCC(SUCC(SUCC(SUCC(ZERO)))), SUCC(SUCC(ZERO)))
let b2 = inverter b1
let _= print_endline(string_of_int b2)
*)

