type formula = TRUE
  |FALSE
  |NOT of formula
  |ANDALSO of formula * formula
  |ORELSE of formula * formula
  |IMPLY of formula * formula
  |LESS of expr * expr
and expr = NUM of int
  |PLUS of expr * expr
  |MINUS of expr * expr

let rec plusminus x = match x with
  |PLUS (a,b) -> (plusminus a) + (plusminus b)
  |MINUS (a, b) -> (plusminus a)-(plusminus b)
  |NUM (a) -> a


let rec eval_help f = match f with
  |FALSE -> FALSE
  |TRUE -> TRUE
  |NOT value -> (match value with
      |TRUE -> FALSE
      |FALSE -> TRUE
      |_ -> eval_help(NOT(eval_help value)))
  |ANDALSO (value,value1)->
      (match value with
        |TRUE ->
          (match value1 with
            |TRUE -> TRUE
            |FALSE -> FALSE
            |_ -> eval_help(ANDALSO((eval_help value), (eval_help value1))))
        |FALSE -> FALSE
        |_ -> eval_help(ANDALSO((eval_help value), (eval_help value1))))
  |ORELSE (value, value1) ->
      (match value with
        |TRUE -> TRUE
        |FALSE ->
          (match value1 with
            |TRUE -> TRUE
            |FALSE -> FALSE
            |_ -> eval_help(ORELSE((eval_help value), (eval_help value1))))
        |_ -> eval_help(ORELSE((eval_help value), (eval_help value1))))
  |IMPLY (value, value1) ->
      (match value with
        |FALSE -> TRUE
        |TRUE ->
          (match value1 with
            |FALSE -> FALSE
            |TRUE -> TRUE
            |_ -> eval_help(IMPLY(eval_help(value), eval_help(value1))))
        |_ -> eval_help (IMPLY(eval_help(value), eval_help(value1))))
  |LESS (value, value1) ->
    (match value, value1 with
      |a, b -> if ((plusminus a)<(plusminus b)) then TRUE else FALSE )

let eval f = let returned = eval_help f in match returned with
  |TRUE -> true
  |FALSE -> false

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
  test_case(22, true = eval (ANDALSO (NOT FALSE, TRUE)));
  test_case(23, true = eval (ORELSE (IMPLY(LESS (NUM (-10), NUM (-100)), ANDALSO (NOT TRUE, TRUE)), ANDALSO (TRUE, ANDALSO (LESS (NUM 10, PLUS (MINUS (NUM 10, NUM (-10)), NUM 30)), TRUE)))));
  test_case (24, true = eval (IMPLY(LESS (NUM (-10), NUM (-100)), ANDALSO (NOT TRUE, TRUE))));
  test_case (25, false = eval(LESS(NUM (100), NUM (10))));
  test_case (26, false = eval(ANDALSO(NOT TRUE, TRUE)));
  test_case (27, false = eval(NOT(NOT FALSE)));
