type formula = True
|False
|Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp

and  exp = Num of int
                   | Plus of exp * exp
                   | Minus of exp * exp

let rec cal : exp -> int = fun f ->
match f with
| Num i -> i
| Plus (i1, i2) -> (cal i1) + (cal i2)
| Minus (i1, i2) -> (cal i1) - (cal i2)

let rec eval : formula -> bool = fun f ->
match f with
|True -> true
|False -> false
|Not nf -> not (eval nf)
|AndAlso (f1, f2) -> (eval f1) && (eval f2)
|OrElse (f1, f2) -> (eval f1) || (eval f2)
|Imply (f1, f2) -> not (eval f1) || (eval f2)
|Equal (e1, e2) -> 
	if((cal e1) - (cal e2)) = 0 then
		true
	else
		false
(*
let _ =
let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case(1, true = eval True); 
  test_case(2, false = eval False); 
  test_case(3, false = eval (Not True)); 
  test_case(4, true = eval (Not False)); 
  test_case(5, true = eval (AndAlso (True, True))); 
  test_case(6, false = eval (AndAlso (True, False))); 
  test_case(7, false = eval (AndAlso (False, True))); 
  test_case(8, false = eval (AndAlso (False, False))); 
  test_case(9, true = eval (OrElse (True, True))); 
  test_case(10, true = eval (OrElse (True, False))); 
  test_case(11, true = eval (OrElse (False, True))); 
  test_case(12, false = eval (OrElse (False, False))); 
  test_case(13, false = eval (Imply (True, False))); 
  test_case(14, true = eval (Imply (True, True))); 
  test_case(15, true = eval (Imply (False, True))); 
  test_case(16, true = eval (Imply (False, False))); 
  test_case(17, true = eval (Equal (Num 3, Num 5))); 
  test_case(18, false = eval (Equal (Num 3, Num 3))); 
  test_case(19, false = eval (Equal (Num 3, Num 1))); 
  test_case(20, false = eval (Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
  test_case(21, true = eval (Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13)))));
*)
