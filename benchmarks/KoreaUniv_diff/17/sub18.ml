
(*Problem 4*)
type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	match e with
	|Const n -> Const 0;
	|Var _ -> Const 1;
	|Power ( _ , 2 ) -> Times [ Const 2 ; Var "x" ]
	|Power ( _ , n ) -> Times [ Const n ; Power ( "x" , (n-1)) ]
	|Times [] -> raise (Failure "list is too short")  
	|Times [ Const n ; Var _ ] -> Const n
	|Times [ Const n ; Power ( _ , 2 ) ] -> Times [ Const ( 2 * n ) ; Var "x" ]
	|Times [ Const n1 ; Power ( _ , n2 ) ] -> Times [ Const ( n1 * n2 ) ; Power ( "x" , (n2 - 1))]
	|Times al -> Times al
	|Sum [] -> raise (Failure "list is too short")
	|Sum [aexp] -> diff(aexp, "x")	
	|Sum al -> Sum [diff((List.hd al), "x") ; diff((Sum (List.tl al)) , "x")]

