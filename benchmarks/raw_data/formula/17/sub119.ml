type formula = True
  |False
  |Not of formula
  |AndAlso of formula * formula
  |OrElse of formula * formula
  |Imply of formula * formula
  |Equal of exp * exp
and exp = Num of int
  |Plus of exp * exp
  |Minus of exp * exp

let rec plusminus x = match x with
  |Plus (a,b) -> (plusminus a) + (plusminus b)
  |Minus (a, b) -> (plusminus a)-(plusminus b)
  |Num (a) -> a


let rec eval_help f = match f with
  |False -> False
  |True -> True
  |Not value -> (match value with
      |True -> False
      |False -> True
      |_ -> eval_help(Not(eval_help value)))
  |AndAlso (value,value1)->
      (match value with
        |True ->
          (match value1 with
            |True -> True
            |False -> False
            |_ -> eval_help(AndAlso((eval_help value), (eval_help value1))))
        |False -> False
        |_ -> eval_help(AndAlso((eval_help value), (eval_help value1))))
  |OrElse (value, value1) ->
      (match value with
        |True -> True
        |False ->
          (match value1 with
            |True -> True
            |False -> False
            |_ -> eval_help(OrElse((eval_help value), (eval_help value1))))
        |_ -> eval_help(OrElse((eval_help value), (eval_help value1))))
  |Imply (value, value1) ->
      (match value with
        |False -> True
        |True ->
          (match value1 with
            |False -> False
            |True -> True
            |_ -> eval_help(Imply(eval_help(value), eval_help(value1))))
        |_ -> eval_help (Imply(eval_help(value), eval_help(value1))))
  |Equal (value, value1) ->
    (match value, value1 with
      |a, b -> if ((plusminus a)=(plusminus b)) then True else False )

let eval f = let returned = eval_help f in match returned with
  |True -> true
  |False -> false
  |_ -> false

(* let _ =
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
  test_case(22, true = eval (AndAlso (Not False, True)));
  test_case(23, true = eval (OrElse (Imply(Equal (Num (-10), Num (-100)), AndAlso (Not True, True)), AndAlso (True, AndAlso (Equal (Num 10, Plus (Minus (Num 10, Num (-10)), Num 30)), True)))));
  test_case (24, true = eval (Imply(Equal (Num (-10), Num (-100)), AndAlso (Not True, True))));
  test_case (25, false = eval(Equal(Num (100), Num (10))));
  test_case (26, false = eval(AndAlso(Not True, True)));
  test_case (27, false = eval(Not(Not False))); *)
