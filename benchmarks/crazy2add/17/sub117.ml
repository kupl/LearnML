type crazy2 = NIL
  |ZERO of crazy2
  |ONE of crazy2
  |MONE of crazy2

(* code from hw2 ----------------*)
let rec crazy2value crazy = match crazy with
  |ZERO x -> 0 :: (crazy2value x)
  |ONE x -> 1 :: (crazy2value x)
  |MONE x -> -1 :: (crazy2value x)
  |NIL -> []

let exp (a:int) (b:int): int = (float_of_int a)**(float_of_int b) |> int_of_float

let crazy_length crazy = List.length(crazy)


let rec crazy2val_help value (counter:int) (length:int) =
  if counter < length then (
    match value with
      |ZERO x -> 0*(exp 2 counter) + (crazy2val_help x (counter+1) length)
      |ONE x -> 1*(exp 2 counter) + (crazy2val_help x (counter+1) length)
      |MONE x -> (-1)*(exp 2 counter) + (crazy2val_help x (counter+1) length)
      |NIL -> 0
      )
      else 0

let crazy2val input = crazy2val_help (input) (0) (crazy_length(crazy2value(input)))



let rec crazy2add (num1,num2)  = match num1, num2 with
  |(ZERO x, ZERO y) -> ZERO(crazy2add(x,y))
  |(ZERO x, ONE y)|(ONE x, ZERO y) ->ONE(crazy2add(x,y))
  |(MONE x,ZERO y)|(ZERO x,MONE y) ->MONE(crazy2add(x,y))
  |(MONE x,ONE y)|(ONE x,MONE y) ->ZERO(crazy2add(x,y))
  |(ONE x, ONE y) ->ZERO(crazy2add(ONE(NIL),crazy2add(x,y)))
  |(MONE x, MONE y) ->ZERO(crazy2add(MONE(NIL),crazy2add(x,y)))
  |(something, NIL)|(NIL, something) -> something



(* let mtwo = ZERO(ONE(MONE NIL))
let one = ONE(NIL)
let five = ONE(ZERO(ONE NIL))
let mone = ONE(MONE NIL)
let mnine = ONE(MONE(ZERO(MONE NIL)))
let zero = ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO NIL)))))))
let five2 = ONE(ZERO(ONE(ZERO(ZERO(ZERO(ZERO(ZERO NIL)))))))
let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL)))))))))
let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL)))))))))

let _ =
  let test_case : int * int * int -> unit = fun (n, x, y) ->
    let result : int * int -> string = fun(x, y) ->
      if(x == y) then "Pass"
      else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in
  let test_nocrazy2val : bool -> unit = fun x ->
    let detector = fun x -> if(x = true) then "Pass" else "crazy2val detected" in
    print_endline ("crazy2val detector : " ^ detector(x)) in
  test_nocrazy2val (crazy2add(zero, five) = five2);
  test_case (1, crazy2val(crazy2add(mnine, mtwo)), crazy2val(crazy2add(mtwo, mnine)));
  test_case (2, crazy2val(mtwo) + crazy2val(mnine), crazy2val(crazy2add(mtwo, mnine)));
  test_case (3, crazy2val(five) + crazy2val(mnine), crazy2val(crazy2add(five, mnine)));
  test_case (4, crazy2val(crazy2add(mnine, one)) + crazy2val(five), crazy2val(mnine) + crazy2val(crazy2add(one, five)));
  test_case (5, crazy2val(crazy2add(crazy2add(mnine, mtwo), crazy2add(five, mtwo))), crazy2val(mnine) + crazy2val(mtwo) + crazy2val(five) + crazy2val(mtwo));
  test_case (6, crazy2val(crazy2add(zero, mnine)), crazy2val(mnine));
  test_case (7, crazy2val(crazy2add(big1, big2)), crazy2val(crazy2add(big2, big1)));
  test_case (8, crazy2val(crazy2add(big1, crazy2add(big1, big2))), crazy2val(crazy2add(crazy2add(big1, big1), big2))); *)
