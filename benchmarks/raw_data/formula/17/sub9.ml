(*print_endline "p1" ;;*)

type formula =
  True
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp =
  Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval_exp (e: exp): int =
  match e with
  Num i -> i
  | Plus (e1, e2) -> eval_exp e1 + eval_exp e2
  | Minus (e1, e2) -> eval_exp e1 - eval_exp e2

let rec eval (f: formula): bool =
  match f with
  | True -> true
  | Not a -> not (eval a)
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) ->
      let evalA = eval a in
      let evalB = eval b in
      if evalA then evalB
      else true
  | Equal (a, b) -> eval_exp a = eval_exp b

(*let _ =*)
  (*let paren str = "(" ^ str ^ ")" in*)
  (*let rec*)
    (*string_of_exp (e: exp) =*)
      (*match e with*)
    (*| Num i -> "INT " ^ (string_of_int i)*)
    (*| Plus (e1, e2) -> string_of_biexp e1 "Plus" e2*)
    (*| Minus (e1, e2) -> string_of_biexp e1 "Minus" e2*)
  (*and*)
    (*string_of_biexp (e1: exp) (op: string) (e2: exp) =*)
      (*let e1_string = e1 |> string_of_exp |> paren in*)
      (*let e2_string = e2 |> string_of_exp |> paren in*)
      (*e1_string ^ " " ^ op ^ " " ^ e2_string*)
  (*in*)

  (*let rec string_of_formula (f: formula) =*)
    (*match f with*)
    (*| True -> "True"*)
    (*| Not f -> "Not " ^ string_of_formula f*)
    (*| AndAlso (f, g) -> string_of_biformula f "AndAlso" g*)
    (*| OrElse (f, g) -> string_of_biformula f "OrElse" g*)
    (*| Imply (f, g) -> string_of_biformula f "Imply" g*)
    (*| Equal (e1, e2) -> string_of_biexp e1 "Equal" e2*)
  (*and*)
    (*string_of_biformula (f: formula) (op: string) (g: formula) =*)
        (*let f_string = f |> string_of_formula |> paren in*)
        (*let g_string = g |> string_of_formula |> paren in*)
        (*f_string ^ " " ^ op ^ " " ^ g_string*)
  (*in*)

  (*let string_of_bool b = if b then "True" else "False" in*)

  (*let assert_equal (expected: bool) (actual: formula) =*)
    (*let eval_actual = eval actual in*)
    (*if eval_actual = expected then print_endline "true"*)
    (*else*)
      (*let message = "Expected " ^ string_of_bool expected ^*)
        (*" but actual " ^ string_of_bool eval_actual ^*)
        (*" from " ^ string_of_formula actual*)
      (*in print_endline message*)
  (*in*)
  (*let not' x = Not x in*)

  (*let t = True |> not' |> not' in*)
  (*let f = not' True in*)
  (*assert_equal true t;*)
  (*assert_equal false f;*)

  (*let andalso x y = AndAlso (x, y) in*)
  (*andalso t t |> assert_equal true;*)
  (*andalso t f |> assert_equal false;*)
  (*andalso f t |> assert_equal false;*)
  (*andalso f f |> assert_equal false;*)

  (*let orelse x y = OrElse (x, y) in*)
  (*orelse t t |> assert_equal true;*)
  (*orelse t f |> assert_equal true;*)
  (*orelse f t |> assert_equal true;*)
  (*orelse f f |> assert_equal false;*)

  (*let imply x y = Imply (x, y) in*)
  (*imply t t |> assert_equal true;*)
  (*imply t f |> assert_equal false;*)
  (*imply f t |> assert_equal true;*)
  (*imply f f |> assert_equal true;*)

  (*let n a = Num a in*)
  (*let less x y = Equal (x, y) in*)
  (*less (n 1) (n 3) |> assert_equal true;*)
  (*less (n 7) (n 0) |> assert_equal false;*)

  (*let plus x y = Plus (Num x, Num y) in*)
  (*less (plus 3 5) (n 4) |> assert_equal false;*)
  (*less (plus 1 (-1)) (n 4) |> assert_equal true;*)
  (*less (plus 1 (-1)) (plus 3 5) |> assert_equal true;*)

  (*let minus x y = Minus (Num x, Num y) in*)
  (*less (minus 3 1) (n 3) |> assert_equal true;*)
  (*less (minus 3 1) (minus 100 1) |> assert_equal true;*)
  (*less (minus 300 1) (minus 100 1) |> assert_equal false;*)
