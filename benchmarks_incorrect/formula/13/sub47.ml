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

let rec calc ex =
  match ex with
    NUM f -> f
  | PLUS (a, b) -> calc a + calc b
  | MINUS (a, b) -> calc a - calc b


let rec eval e =
  match e with
    NOT x -> (fun x -> if (x == TRUE) then false else true) x
  | ANDALSO(a,b) -> if (a == FALSE) then false else if (b == FALSE) then false else true
  | ORELSE(a,b) -> if (a == TRUE) then true else if (b == TRUE) then true else false
  | IMPLY(a,b) -> if ((a == FALSE) && (b == TRUE)) then false else true
  | LESS(a, b)-> calc(a)<calc(b)  (* returns true if exp of left side is smaller than right one *)
  | FALSE -> false
  | TRUE -> true


(*
let _ = if (eval( LESS( NUM(5), PLUS(NUM(10),NUM(2)) ) ) == false) then print_string("test")
*)