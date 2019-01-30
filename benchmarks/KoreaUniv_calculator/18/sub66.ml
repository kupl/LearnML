exception Unvalid_Multiple_Sigma;;
(* Since there is only one variable "X" prepared for sigma, I have ruled out the overlapping sigma. *)
exception Unvalid_X_Without_Sigma;;
(* "X" always should be used with sigma. *)

type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec mysig : exp -> exp -> exp
= fun var eq ->
    match eq with
      | X -> var
      | INT e -> INT e
      | ADD (e1, e2) -> ADD ((mysig var e1), (mysig var e2))
      | SUB (e1, e2) -> SUB ((mysig var e1), (mysig var e2))
      | MUL (e1, e2) -> MUL ((mysig var e1), (mysig var e2))
      | DIV (e1, e2) -> MUL ((mysig var e1), (mysig var e2))
      | SIGMA (e1, e2, e3) -> raise Unvalid_Multiple_Sigma;;

let rec calculator : exp -> int
= fun exp ->
    match exp with
      | X -> raise Unvalid_X_Without_Sigma
      | INT e -> e
      | ADD (e1, e2) -> (calculator e1) + (calculator e2)
      | SUB (e1, e2) -> (calculator e1) - (calculator e2)
      | MUL (e1, e2) -> (calculator e1) * (calculator e2)
      | DIV (e1, e2) -> (calculator e1) / (calculator e2)
      | SIGMA (e1, e2, e3) ->
          if ((calculator e1) > (calculator e2)) then 0
          else (calculator (ADD ((mysig e1 e3), SIGMA ((ADD (e1, INT 1)), e2, e3))));;

(*
calculator (SIGMA(INT 1, INT 3, SUB(MUL(X, X), INT 1)));;
*)
