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
  |Num a -> a


let rec eval f = match f with
  |False -> false
  |True -> true
  |Not value -> not (eval value)
  |AndAlso (value,value1)->
      (match value with
        |True ->
          (match value1 with
            |True -> true
            |False -> false
            |_ -> eval value1)
        |False -> false
        |_ -> eval value)
  |OrElse (value, value1) ->
      (match value with
        |True -> true
        |False ->
          (match value1 with
            |True -> true
            |False -> false
            |_ -> eval value1)
        |_ -> eval value)
  |Imply (value, value1) ->
      (match value with
        |True ->
          (match value1 with
            |False -> false
            |True -> true
            |_ -> eval value1)
        |False -> true
        |_ -> eval value)
  |Equal (value, value1) ->
    (match value, value1 with
      |a, b -> if (plusminus a)=(plusminus b) then true else false )


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
  test_case(22, true = eval (AndAlso (Not False, True))) *)
