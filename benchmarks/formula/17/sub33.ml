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

let rec calc:expr->int = fun expr ->
    match expr with
    |NUM a -> a
    |PLUS (a, b) -> (match (a,b) with
                    |(NUM a, NUM b) -> a + b
                    |(_, _) -> 0)
    |MINUS (a, b) -> (match (a,b) with
                     |(NUM a, NUM b) -> a - b
                     |(_, _) -> 0)
    
let rec eval:formula->bool = fun formula ->
    match formula with
    |TRUE -> true
    |FALSE -> false
    |NOT a -> (match a with
              |TRUE -> false
              |FALSE -> true
              |_ -> false)
    |ANDALSO (a,b) -> (match (a,b) with
                      |(TRUE,TRUE) -> true
                      |(TRUE,FALSE) -> false
                      |(FALSE,TRUE) -> false
                      |(FALSE,FALSE) -> false
                      |(_,_) -> false)

    |ORELSE (a,b) -> (match (a,b) with
                      |(TRUE,TRUE) -> true
                      |(TRUE,FALSE) -> true
                      |(FALSE,TRUE) -> true
                      |(FALSE,FALSE) -> false
                      |(_,_) -> false)

    |IMPLY (a,b) -> (match (a,b) with
                      |(TRUE,TRUE) -> true
                      |(TRUE,FALSE) -> false
                      |(FALSE,TRUE) -> true
                      |(FALSE,FALSE) -> true
                      |(_,_) -> false)

    |LESS (a, b) -> if calc a < calc b then true
                    else false
(*
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
                                                          test_case(18, false = eval (LESS (NUM 2, NUM 2))); 
                                                            test_case(19, false = eval (LESS (NUM 3, NUM 1))); 
                                                              test_case(20, false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
                                                                test_case(21, true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13)))));
                                                                *)
