(**********************)
(*   Problem 1        *)
(**********************)

type exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* You can define datatypes and helper functions as necessary *)

let rec eval : string -> exp -> exp -> bool
= fun var exp1 exp2  -> match exp2 with
                          | CONST integer -> false
                          | VAR x -> if (x = var) then true else false
                          | ADD (exp3, exp4) -> if (((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) then false else true
                          | SUB (exp3, exp4) -> if (((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) then false else true
                          | MUL (exp3, exp4) -> if (((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) then false else true
                          | DIV (exp3, exp4) -> if (((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) then false else true
                          | ISZERO exp3 -> eval var exp1 exp3
                          | IF (exp3, exp4, exp5) -> if ((((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) && ((eval var exp1 exp5))) then false else true
                          | LETREC (var1, var2, exp3, exp4) -> if (((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) then false else true
                          | PROC (var1, exp3) -> eval var exp1 exp3
                          | CALL (exp3, exp4) -> if (((eval var exp1 exp3) = false) && ((eval var exp1 exp4) = false)) then false else true


let rec final : exp -> exp 
= fun exp -> match exp with
              | LET (var, exp1, exp2) ->
                                      (match exp2 with
                                        | CONST integer -> CONST integer
                                        | VAR x -> if (x = var) then exp1  else VAR x
                                        | ADD (exp3, exp4) -> ADD (final (LET (var, exp1, exp3)), final (LET (var, exp1, exp4)))
                                        | SUB (exp3, exp4) -> SUB (final (LET (var, exp1, exp3)), final (LET (var, exp1, exp4)))
                                        | MUL (exp3, exp4) -> MUL (final (LET (var, exp1, exp3)), final (LET (var, exp1, exp4)))
                                        | DIV (exp3, exp4) -> DIV (final (LET (var, exp1, exp3)), final (LET (var, exp1, exp4)))
                                        | ISZERO exp3 -> ISZERO (final (LET (var, exp1, exp3)))
                                        | IF (exp3, exp4, exp5) -> IF (final (LET (var, exp1, exp3)), final (LET (var, exp1, exp4)), final (LET (var, exp1, exp5)))
                                        | LET (var1, exp3, exp4) ->  LET (var1, final (LET(var, exp1, exp3)), final (LET(var, exp1, exp4)))
                                        | LETREC (var1, var2, exp3, exp4) -> LETREC (var1, var2, final (LET(var, exp1, exp3)), final (LET (var, exp1, exp4)))    
                                        | PROC (var1, exp3) -> PROC (var1, final (LET (var, exp1, exp3)))
                                        | CALL (exp3, exp4) -> CALL (final (LET (var, exp1, exp3)), final (LET (var, exp1, exp4)))

                                      )


let rec expand : exp -> exp
= fun exp -> match exp with
              | LET (var, exp1, exp2) -> if (eval var exp1 exp2) = true then (final exp) else exp


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> true (* TODO *)
