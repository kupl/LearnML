type formula = TRUE
        | FALSE
        | NOT of formula
        | ANDALSO of formula * formula
        | ORELSE of formula * formula
        | IMPLY of formula * formula
        | LESS of expr * expr
    and  expr = NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
let rec eval_expr e =
	match e with
		| NUM x -> x
		| PLUS (e_1, e_2) -> (eval_expr e_1) + (eval_expr e_2)
		| MINUS (e_1, e_2) -> (eval_expr e_1) - (eval_expr e_2)
let rec eval f =
	match f with
		| TRUE -> true
		| FALSE -> false
		| NOT f_ -> not (eval f_)
		| ANDALSO (f_1, f_2) -> (eval f_1) && (eval f_2)
		| ORELSE (f_1, f_2) -> (eval f_1) || (eval f_2)
		| IMPLY (f_1, f_2) -> (eval (ANDALSO (f_1, f_2)) || (eval (NOT f_1)))
		| LESS (e_1, e_2) -> (eval_expr e_1) < (eval_expr e_2)
	
(* TEST SET*)
(*
let _ =
    print_string "HW6 Test Set\n";
    Printf.printf ("%b") (eval (NOT TRUE));
    print_newline ();
    Printf.printf ("%b") (eval (ANDALSO(LESS(NUM 10, NUM 5), LESS(PLUS (NUM 4, NUM 5) , MINUS (NUM 50, NUM 1)))));
    print_newline ();
    Printf.printf ("%b") (eval (NOT (ANDALSO (LESS (NUM 0, NUM 0), TRUE))));
    print_newline ();
    Printf.printf ("%b") (eval (IMPLY(LESS (NUM 0, NUM 1), ANDALSO(LESS (NUM 0, NUM 0), NOT TRUE))));
    print_newline ();
    Printf.printf ("%b") (eval (IMPLY(LESS (NUM 1, NUM 0), ORELSE(ANDALSO(TRUE, FALSE), ORELSE(NOT TRUE, LESS(NUM 1, NUM 2))))));
    print_newline ()
*)