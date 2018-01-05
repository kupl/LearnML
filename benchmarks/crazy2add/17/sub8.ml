print_endline "hw2 e3 2010-11969";;

let a = 3

type crazy2 = NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val (c: crazy2): int =
  match c with
  | NIL -> 0
  | ZERO c' -> 2 * crazy2val c'
  | ONE c' -> 2 * crazy2val c' + 1
  | MONE c' -> 2 * crazy2val c' - 1

exception Invalid_state of string

(* ASSERT: c is not nil *)
let splitHeadAndTail (c: crazy2): int * crazy2 =
  match c with
  | NIL -> raise (Invalid_state "Invalid NIL")
  | ZERO c' -> (0, c')
  | ONE c' -> (1, c')
  | MONE c' -> (-1, c')

let rec crazy2add' (c1: crazy2) (c2:crazy2) (carry: int): crazy2 =
  match (c1, c2, carry) with
  | (NIL, _, 0) -> c2
  | (NIL, _, 1) -> crazy2add' c2 (ONE NIL) 0
  | (NIL, _, -1) -> crazy2add' c2 (MONE NIL) 0
  | (_, NIL, 0) -> c1
  | (_, NIL, 1) -> crazy2add' c1 (ONE NIL) 0
  | (_, NIL, -1) -> crazy2add' c1 (MONE NIL) 0
  | _ ->
    let (h1, t1) = splitHeadAndTail c1 in
    let (h2, t2) = splitHeadAndTail c2 in
    match (carry + h1 + h2) with
    | 3 -> ONE (crazy2add' t1 t2 1)
    | 2 -> ZERO (crazy2add' t1 t2 1)
    | 1 -> ONE (crazy2add' t1 t2 0)
    | 0 -> ZERO (crazy2add' t1 t2 0)
    | -1 -> MONE (crazy2add' t1 t2 0)
    | -2 -> ZERO (crazy2add' t1 t2 (-1))
    | -3 -> MONE (crazy2add' t1 t2 (-1))
    | _ -> raise (Invalid_state "Invalid calculation result")

let rec crazy2add ((c1: crazy2), (c2: crazy2)): crazy2 =
  crazy2add' c1 c2 0

let _ =
  let rec string_of_crazy2 (c: crazy2): string =
    match c with
    | ZERO c' -> "0" ^ string_of_crazy2 c'
    | ONE c' -> "+" ^ string_of_crazy2 c'
    | MONE c' -> "-" ^ string_of_crazy2 c'
    | _ -> ""
  in
  let assert_equal (expected: int) (c: crazy2) =
    if crazy2val c = expected then print_endline "true"
    else
      Printf.printf "Expected %d but actual %d from %s\n"
        expected (crazy2val c) (string_of_crazy2 c)
  in
  let test_add (x: crazy2) (y: crazy2) =
    let xVal = crazy2val x in
    let yVal = crazy2val y in
    crazy2add (x, y) |> assert_equal (xVal + yVal)
  in

  let zero n = ZERO n in
  let one n = ONE n in
  let mone n = MONE n in

  let six = NIL |> one |> one |> zero in
  assert_equal 6 six;
  let two = NIL |> one |> mone |> zero in
  assert_equal 2 two;
  let thirteen = NIL |> one |> mone |> one |> one |> mone in
  assert_equal 13 thirteen;

  test_add six two;
  test_add six thirteen;
  test_add two thirteen;
  test_add (ZERO NIL) six;
  test_add NIL six;
  test_add six (ZERO NIL);
  test_add six NIL;

  print_endline "end";
