(* hw2-2 *)
(* 2010-11687 Keunjun Choi *)

type aexp = Const of int
    | Var of string
    | Power of string * int
    | Times of aexp list
    | Sum of aexp list

let rec diff (expr, value) =
    let rec sum_term expr =
	match expr with
	| [] -> []
	| expr1::expr2 ->
	    (diff (expr1, value))::(sum_term expr2)
    in

    let rec time_term (expr, num) =
	let rec dx (expr, count) =
	    match expr with
	    | [] -> []
	    | expr1::expr2 ->
		(if ((num < count) || (num > count)) then
		    (expr1)::(dx (expr2, (count + 1)))
		else
		    match expr1 with
		    | Const opr -> (Const 0)::(dx (expr2, (count + 1)))
		    | Var opr ->
			(if (opr = value) then
			    (Const 1)::(dx (expr2, (count + 1)))
			else (Const 0)::(dx (expr2, (count + 1))))
		    | Power (opr1, opr2) ->
			(if ((opr1 = value) && (opr2 != 0)) then
			    (Const opr2)::(Power (opr1, (opr2 - 1)))::(dx (expr2, count + 1))
			else (Const 0)::(dx (expr2, (count + 1))))
		    | Times opr ->
			(Sum (time_term (opr, 0)))::(dx (expr2, (count + 1)))
		    | Sum opr ->
			(Sum (sum_term opr))::(dx (expr2, (count + 1))))
	in

	(if (num = (List.length expr)) then []
	else (Times (dx (expr, 0)))::(time_term (expr, (num + 1))))
    in

    match expr with
    | Const opr -> (Const 0)
    | Var opr ->
	(if (opr = value) then
	    (Const 1)
	else (Const 0))
    | Power (opr1, opr2) ->
	(if ((opr1 = value) && (opr2 != 0)) then
	    (Times ((Const opr2)::(Power (opr1, (opr2 - 1))::[])))
	else (Const 0))
    | Times opr -> (Sum (time_term (opr, 0)))
    | Sum opr -> (Sum (sum_term (opr)))
