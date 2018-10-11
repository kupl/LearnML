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
exception TypeError

let empty = []
module Variables  = struct
  type v = (var * exp) list
  let rec checkVar varx data =
    (match varx with
    |VAR(x) -> (match data with
              |hd::tl -> (match hd with
                        |(v, e) -> if v=x then true else checkVar varx tl)
              |[] -> false)
    |_ -> raise TypeError)
  let rec give varx data = 
    (match varx with
    |VAR(x) -> (match data with
              |hd::tl -> (match hd with
                        |(v, e) -> if v=x then e else give varx tl)
              |[] -> raise TypeError)
    |_ -> raise TypeError)
  let rec delete x data = List.map (fun d -> (match d with
                                            |(v, e) -> if v = x then (" ffff", e) else d)) data
end

let rec check v e =
    let cnt = ref 0 in

    let rec plz v e =
      match e with
      |VAR(x) -> if (v=x) then cnt := !cnt + 1
      |ADD(e1, e2) |SUB(e1,e2) | MUL(e1,e2) |DIV(e1,e2) -> plz v e1; plz v e2
      |ISZERO(e1) -> plz v e1
      |IF(e1, e2, e3) -> plz v e1; plz v e2; plz v e3
      |LET(x, e1, e2) -> if not (v=x) then (plz v e1; plz v e2)
      |LETREC(f, x, e1, e2) -> if (v=f) then (cnt := 0) else if not (v=x) then (plz v e1; plz v e2) else plz v e2 
      |PROC(x, e1) -> if not (v=x) then plz v e1 
      |CALL(e1, e2) -> plz v e1; plz v e2
      |_ -> cnt := !cnt

  in plz v e; if not (!cnt = 0) then true else false

let rec expand : exp -> exp 
= fun exp -> 

  let rec test : Variables.v -> exp -> exp
  = fun v e ->
    match e with
    |CONST(n) -> e
    |VAR(x) -> if (Variables.checkVar e v) then test v (Variables.give e v) else e
    |ADD(e1, e2) ->  ADD(test v e1, test v e2)
    |SUB(e1, e2) -> SUB(test v e1, test v e2)
    |MUL(e1, e2) -> MUL(test v e1, test v e2)
    |DIV(e1, e2) -> DIV(test v e1, test v e2)
    |ISZERO(e1) -> ISZERO(test v e1)
    |READ -> READ
    |IF(e1, e2, e3) -> IF(test v e1, test v e2, test v e3)
    |LET(x, e1, e2) -> if check x e2 then test ([x, e1]@v) e2 
                        else LET(x, test v e1, test v e2)
    |LETREC(f, x, e1, e2) -> if (Variables.checkVar (VAR (x)) v) then (let v' = (Variables.delete x v) in LETREC(f, x, test v' e1, test ([f, e]@v) e2))
                              else LETREC(f, x, test v e1, test ([f, e]@v) e2)
    |PROC(x, e1) -> PROC(x, test v e1)
    |CALL(e1, e2) -> CALL(test v e1, test v e2)

  in
  test empty exp

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec eval l env =
    match l with
    |V(x) -> (match env with
            |[] -> false
            |hd::tl -> if hd=x then true else eval l tl)
    |P(x, l1) -> eval l1 ([x]@env)
    |C(l1, l2) -> (eval l1 env) && (eval l2 env)
  
  in
  eval lam empty




