let merge ((left, right) : int list * int list) : int list =
  let negCompare a b = ~- (compare a b) in
  List.merge negCompare left right

let rec sigma ((a, b, f) : int * int * (int -> int)) : int =
  if a > b then 0
  else f a + sigma (a + 1, b, f)

let rec iter ((n, f) : int * ('a -> 'a)) : 'a -> 'a =
  let comp f g x = f (g x) in
  if n <= 0 then fun x -> x
  else comp f (iter (n - 1, f))

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

let rec calc (e : expr) : int =
  match e with
    NUM(n) -> n
  | PLUS(subExpr1, subExpr2) -> calc subExpr1 + calc subExpr2
  | MINUS(subExpr1, subExpr2) -> calc subExpr1 - calc subExpr2

let rec eval (f : formula) : bool =
  match f with
    TRUE -> true
  | FALSE -> false
  | NOT(subForm) -> not @@ eval subForm
  | ANDALSO(subForm1, subForm2) -> eval subForm1 && eval subForm2
  | ORELSE(subForm1, subForm2) -> eval subForm1 || eval subForm2
  | IMPLY(subForm1, subForm2) -> not (eval subForm1) || eval subForm2
  | LESS(subExpr1, subExpr2) -> calc subExpr1 < calc subExpr2

type nat = ZERO | SUCC of nat

let rec natadd ((a, b) : nat * nat) : nat =
  match a with
    ZERO -> b
  | SUCC(n) -> natadd (n, SUCC(b))

let rec natmul ((a, b) : nat * nat) : nat =
  match a with
    ZERO -> ZERO
  | SUCC(n) -> natadd (a, natmul(n, b))
