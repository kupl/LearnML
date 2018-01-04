(* 2011-10915 / 생명과학부 / 신지민/ Homework 1-5 *)

type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (n1, n2) ->
	match n1 with	
	|ZERO -> n2
	|SUCC(tl1) -> 
	 	(match n2 with
		|ZERO -> n1
		|SUCC(tl2) -> natadd(tl1,SUCC(n2))
		)

let rec natmul : nat * nat -> nat = fun (n1, n2) ->
	match n1 with
	|ZERO -> ZERO
	|SUCC(ZERO) -> n2
	|SUCC(tl1) -> 
		(match n2 with
		|ZERO -> ZERO
		|SUCC(ZERO) -> n1
		|SUCC(tl2) -> natadd(n2,natmul(tl1,n2))
		)

(*
let rec print = fun n ->
	match n with
	|ZERO -> print_endline("zero ")
	|SUCC(tl1) -> begin
			 print tl1;
		 	 print_endline("1 ")
		      end
let a = print(natadd(SUCC(SUCC(SUCC(ZERO))),ZERO))
let _= print_endline("DONE\n")
let a = print(natadd(ZERO,SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(ZERO))))))))
let _= print_endline("DONE\n")
let a = print(natadd(ZERO,ZERO))
let _= print_endline("DONE\n")
let a = print(natmul(SUCC(SUCC(ZERO)),SUCC(SUCC(ZERO))))
let _= print_endline("DONE\n")
let a = print(natmul(SUCC(SUCC(SUCC(ZERO))),SUCC(SUCC(SUCC(ZERO)))))
let _= print_endline("DONE\n")
let a = print(natmul(SUCC(SUCC(SUCC(ZERO))),SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(ZERO)))))))))
*)
