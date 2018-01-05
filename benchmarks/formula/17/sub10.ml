(*print_endline "p1" ;;*)

type formula =
  TRUE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval_expr (e: expr): int =
  match e with
  NUM i -> i
  | PLUS (e1, e2) -> eval_expr e1 + eval_expr e2
  | MINUS (e1, e2) -> eval_expr e1 - eval_expr e2

let rec eval (f: formula): bool =
  match f with
  | TRUE -> true
  | NOT a -> not (eval a)
  | ANDALSO (a, b) -> eval a && eval b
  | ORELSE (a, b) -> eval a || eval b
  | IMPLY (a, b) ->
      let evalA = eval a in
      let evalB = eval b in
      if evalA then evalB
      else true
  | LESS (a, b) -> eval_expr a < eval_expr b

(*let _ =*)
  (*let paren str = "(" ^ str ^ ")" in*)
  (*let rec*)
    (*string_of_expr (e: expr) =*)
      (*match e with*)
    (*| NUM i -> "INT " ^ (string_of_int i)*)
    (*| PLUS (e1, e2) -> string_of_biexpr e1 "PLUS" e2*)
    (*| MINUS (e1, e2) -> string_of_biexpr e1 "MINUS" e2*)
  (*and*)
    (*string_of_biexpr (e1: expr) (op: string) (e2: expr) =*)
      (*let e1_string = e1 |> string_of_expr |> paren in*)
      (*let e2_string = e2 |> string_of_expr |> paren in*)
      (*e1_string ^ " " ^ op ^ " " ^ e2_string*)
  (*in*)

  (*let rec string_of_formula (f: formula) =*)
    (*match f with*)
    (*| TRUE -> "TRUE"*)
    (*| NOT f -> "NOT " ^ string_of_formula f*)
    (*| ANDALSO (f, g) -> string_of_biformula f "ANDALSO" g*)
    (*| ORELSE (f, g) -> string_of_biformula f "ORELSE" g*)
    (*| IMPLY (f, g) -> string_of_biformula f "IMPLY" g*)
    (*| LESS (e1, e2) -> string_of_biexpr e1 "LESS" e2*)
  (*and*)
    (*string_of_biformula (f: formula) (op: string) (g: formula) =*)
        (*let f_string = f |> string_of_formula |> paren in*)
        (*let g_string = g |> string_of_formula |> paren in*)
        (*f_string ^ " " ^ op ^ " " ^ g_string*)
  (*in*)

  (*let string_of_bool b = if b then "TRUE" else "FALSE" in*)

  (*let assert_equal (expected: bool) (actual: formula) =*)
    (*let eval_actual = eval actual in*)
    (*if eval_actual = expected then print_endline "true"*)
    (*else*)
      (*let message = "Expected " ^ string_of_bool expected ^*)
        (*" but actual " ^ string_of_bool eval_actual ^*)
        (*" from " ^ string_of_formula actual*)
      (*in print_endline message*)
  (*in*)
  (*let not' x = NOT x in*)

  (*let t = TRUE |> not' |> not' in*)
  (*let f = not' TRUE in*)
  (*assert_equal true t;*)
  (*assert_equal false f;*)

  (*let andalso x y = ANDALSO (x, y) in*)
  (*andalso t t |> assert_equal true;*)
  (*andalso t f |> assert_equal false;*)
  (*andalso f t |> assert_equal false;*)
  (*andalso f f |> assert_equal false;*)

  (*let orelse x y = ORELSE (x, y) in*)
  (*orelse t t |> assert_equal true;*)
  (*orelse t f |> assert_equal true;*)
  (*orelse f t |> assert_equal true;*)
  (*orelse f f |> assert_equal false;*)

  (*let imply x y = IMPLY (x, y) in*)
  (*imply t t |> assert_equal true;*)
  (*imply t f |> assert_equal false;*)
  (*imply f t |> assert_equal true;*)
  (*imply f f |> assert_equal true;*)

  (*let n a = NUM a in*)
  (*let less x y = LESS (x, y) in*)
  (*less (n 1) (n 3) |> assert_equal true;*)
  (*less (n 7) (n 0) |> assert_equal false;*)

  (*let plus x y = PLUS (NUM x, NUM y) in*)
  (*less (plus 3 5) (n 4) |> assert_equal false;*)
  (*less (plus 1 (-1)) (n 4) |> assert_equal true;*)
  (*less (plus 1 (-1)) (plus 3 5) |> assert_equal true;*)

  (*let minus x y = MINUS (NUM x, NUM y) in*)
  (*less (minus 3 1) (n 3) |> assert_equal true;*)
  (*less (minus 3 1) (minus 100 1) |> assert_equal true;*)
  (*less (minus 300 1) (minus 100 1) |> assert_equal false;*)
