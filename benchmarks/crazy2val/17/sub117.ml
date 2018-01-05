
type crazy2 = NIL
  |ZERO of crazy2
  |ONE of crazy2
  |MONE of crazy2


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

(* let mtwo = ZERO(ONE(MONE NIL))
let one = ONE(NIL)
let five = ONE(ZERO(ONE NIL))
let mone = ONE(MONE NIL)
let mnine = ONE(MONE(ZERO(MONE NIL)))
let zero = ZERO(ZERO(ZERO NIL))
let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL)))))))))
let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) *)


(* let _ =
  let test_case : int * int * int -> unit = fun (n, x, y) ->
    let result : int * int -> string = fun(x, y) ->
      if(x == y) then "Pass"
      else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in
  test_case(1, -2, crazy2val(mtwo));
  test_case(2, 1, crazy2val(one));
  test_case(3, 5, crazy2val(five));
  test_case(4, -1, crazy2val(mone));
  test_case(5, -9, crazy2val(mnine));
  test_case(6, 0, crazy2val(zero));
  test_case(7, 298, crazy2val(big1));
  test_case(8, -437, crazy2val(big2)); *)



(* let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l; print_endline ""

let _ = print_list (crazy2value five) *)
