type formula = True
        | False
        | Not of formula
        | AndAlso of formula * formula
        | OrElse of formula * formula
        | Imply of formula * formula
        | Equal of exp * exp
    and  exp = Num of int
        | Plus of exp * exp
        | Minus of exp * exp
let rec eval_exp e =
	match e with
		| Num x -> x
		| Plus (e_1, e_2) -> (eval_exp e_1) + (eval_exp e_2)
		| Minus (e_1, e_2) -> (eval_exp e_1) - (eval_exp e_2)
let rec eval f =
	match f with
		| True -> true
		| False -> false
		| Not f_ -> not (eval f_)
		| AndAlso (f_1, f_2) -> (eval f_1) && (eval f_2)
		| OrElse (f_1, f_2) -> (eval f_1) || (eval f_2)
		| Imply (f_1, f_2) -> (eval (AndAlso (f_1, f_2)) || (eval (Not f_1)))
		| Equal (e_1, e_2) -> (eval_exp e_1) = (eval_exp e_2)
	
(* TEST SET*)
(*
let _ =
    print_string "HW6 Test Set\n";
    Printf.printf ("%b") (eval (Not True));
    print_newline ();
    Printf.printf ("%b") (eval (AndAlso(Equal(Num 10, Num 5), Equal(Plus (Num 4, Num 5) , Minus (Num 50, Num 1)))));
    print_newline ();
    Printf.printf ("%b") (eval (Not (AndAlso (Equal (Num 0, Num 0), True))));
    print_newline ();
    Printf.printf ("%b") (eval (Imply(Equal (Num 0, Num 1), AndAlso(Equal (Num 0, Num 0), Not True))));
    print_newline ();
    Printf.printf ("%b") (eval (Imply(Equal (Num 1, Num 0), OrElse(AndAlso(True, False), OrElse(Not True, Equal(Num 1, Num 2))))));
    print_newline ()
*)