type formula = TRUE
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr
and expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr

let rec eval: formula -> bool = fun(f) ->
  let rec expr_to_int(x: expr): int =
  match x with
    |NUM i -> i
    |PLUS (a, b) -> expr_to_int(a) + expr_to_int(b)
    |MINUS (a, b) -> expr_to_int(a) - expr_to_int(b) in
  match f with
    |TRUE -> true
    |FALSE -> false
    |NOT a -> not(eval(a))
    |ANDALSO(a,b) -> eval(a) && eval(b)
    |ORELSE(a,b) -> eval(a) || eval(b)
    |IMPLY(a,b) -> not(eval(a)) || eval(b)
    |LESS(a,b) -> expr_to_int(a) < expr_to_int(b)


(* TEST CASES *)
(*
let rec expr_to_int(x: expr): int =
  match x with
    |NUM i -> i
    |PLUS (a, b) -> expr_to_int(a) + expr_to_int(b)
    |MINUS (a, b) -> expr_to_int(a) - expr_to_int(b)


let true1 = TRUE
let true2 = NOT FALSE
let true3 = ANDALSO(true1, true2)
let true4 = ORELSE(true3, FALSE)
let true5 = IMPLY (NOT true4, FALSE)
let true6 = IMPLY (true3, NOT FALSE)
let expr8 = NUM 8
let expr5 = MINUS (expr8, NUM 3)
let expr13 = PLUS (expr8, expr5)
let true7 = LESS (expr8, expr13)

let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case(1, true = eval TRUE); 
  test_case(2, false = eval FALSE); 
  test_case(3, false = eval (NOT TRUE)); 
  test_case(4, true = eval (NOT FALSE)); 
  test_case(5, true = eval (ANDALSO (TRUE, TRUE))); 
  test_case(6, false = eval (ANDALSO (TRUE, FALSE))); 
  test_case(7, false = eval (ANDALSO (FALSE, TRUE))); 
  test_case(8, false = eval (ANDALSO (FALSE, FALSE))); 
  test_case(9, true = eval (ORELSE (TRUE, TRUE))); 
  test_case(10, true = eval (ORELSE (TRUE, FALSE))); 
  test_case(11, true = eval (ORELSE (FALSE, TRUE))); 
  test_case(12, false = eval (ORELSE (FALSE, FALSE))); 
  test_case(13, false = eval (IMPLY (TRUE, FALSE))); 
  test_case(14, true = eval (IMPLY (TRUE, TRUE))); 
  test_case(15, true = eval (IMPLY (FALSE, TRUE))); 
  test_case(16, true = eval (IMPLY (FALSE, FALSE))); 
  test_case(17, true = eval (LESS (NUM 3, NUM 5))); 
  test_case(18, false = eval (LESS (NUM 3, NUM 3))); 
  test_case(19, false = eval (LESS (NUM 3, NUM 1))); 
  test_case(20, false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
  test_case(21, true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13)))));
  test_case(22, true = eval(true1));
  test_case(23, true = eval(true2));
  test_case(24, true = eval(true3));
  test_case(25, true = eval(true4));
  test_case(26, true = eval(true5));
  test_case(27, true = eval(true6));
  test_case(28, true = eval(true7));
  test_case(29, 8 = expr_to_int(expr8));
  test_case(30, 5 = expr_to_int(expr5));
  test_case(31, 13 = expr_to_int(expr13));
  *)
