(*print_endline "hw2 e2 juhyeong 2010-11959";;*)

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

(*let _ =*)
  (*print_endline "END";*)
  (*let rec string_of_crazy2 (c: crazy2): string =*)
    (*match c with*)
    (*| ZERO c' -> "0" ^ string_of_crazy2 c'*)
    (*| ONE c' -> "+" ^ string_of_crazy2 c'*)
    (*| MONE c' -> "-" ^ string_of_crazy2 c'*)
    (*| _ -> ""*)
  (*in*)
  (*let assert_equal (expected: int) (c: crazy2) =*)
    (*if crazy2val c = expected then print_endline "true"*)
    (*else*)
      (*Printf.printf "Expected %d but actual %d from %s\n"*)
        (*expected (crazy2val c) (string_of_crazy2 c)*)
  (*in*)

  (*let zero n = ZERO n in*)
  (*let one n = ONE n in*)
  (*let mone n = MONE n in*)
  (*NIL |> one |> one |> zero |> assert_equal 6;*)
  (*NIL |> one |> mone |> zero |> assert_equal 2;*)
  (*NIL |> one |> mone |> one |> one |> mone |> assert_equal 13;*)

