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

let rec arith f =
  match f with
  |NUM n -> n
  |PLUS (n, m) -> (arith n) + (arith m)
  |MINUS (n, m) -> (arith n) - (arith m)

let rec eval form =
  match form with
  |TRUE -> true
  |FALSE -> false
  |NOT p -> if eval p = true
            then false
            else true
  |ANDALSO (p,q) -> if eval p = true
                  then (if eval q =true
                        then true
                        else false)
                  else false
  |ORELSE (p,q) -> if eval p = true
                  then true
                  else (if eval q =true
                          then true
                          else false)
  |IMPLY (p,q) -> if eval p = true
                  then (if eval q = true
                          then true
                          else false)
                  else true

  |LESS (n, m) -> if arith (MINUS (n,m)) < 0
                  then true
                  else false


  


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
