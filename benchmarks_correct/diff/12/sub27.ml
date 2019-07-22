(* hw2-2 *)
(* 2010-11687 Keunjun Choi *)

type ae = CONST of int
    | VAR of string
    | POWER of string * int
    | TIMES of ae list
    | SUM of ae list

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
		    | CONST opr -> (CONST 0)::(dx (expr2, (count + 1)))
		    | VAR opr ->
			(if (opr = value) then
			    (CONST 1)::(dx (expr2, (count + 1)))
			else (CONST 0)::(dx (expr2, (count + 1))))
		    | POWER (opr1, opr2) ->
			(if ((opr1 = value) && (opr2 != 0)) then
			    (CONST opr2)::(POWER (opr1, (opr2 - 1)))::(dx (expr2, count + 1))
			else (CONST 0)::(dx (expr2, (count + 1))))
		    | TIMES opr ->
			(SUM (time_term (opr, 0)))::(dx (expr2, (count + 1)))
		    | SUM opr ->
			(SUM (sum_term opr))::(dx (expr2, (count + 1))))
	in

	(if (num = (List.length expr)) then []
	else (TIMES (dx (expr, 0)))::(time_term (expr, (num + 1))))
    in

    match expr with
    | CONST opr -> (CONST 0)
    | VAR opr ->
	(if (opr = value) then
	    (CONST 1)
	else (CONST 0))
    | POWER (opr1, opr2) ->
	(if ((opr1 = value) && (opr2 != 0)) then
	    (TIMES ((CONST opr2)::(POWER (opr1, (opr2 - 1))::[])))
	else (CONST 0))
    | TIMES opr -> (SUM (time_term (opr, 0)))
    | SUM opr -> (SUM (sum_term (opr)))
