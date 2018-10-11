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
let rec find: exp -> var -> bool
= fun exp var ->
    match exp with
    | CONST n -> false
    | VAR x -> 
            if var = x then true
            else false
    | ADD (e1, e2) -> 
            let b1 = find e1 var in
            let b2 = find e2 var in
            b1 || b2
    | SUB (e1, e2) ->
            let b1 = find e1 var in
            let b2 = find e2 var in
            b1 || b2
    | MUL (e1, e2) ->
            let b1 = find e1 var in
            let b2 = find e2 var in
            b1 || b2
    | DIV (e1, e2) ->
            let b1 = (e1 = VAR var) in
            let b2 = (e2 = VAR var) in
            b1 || b2
    | READ ->
            false
    | ISZERO e ->
            let b = find e var in b
    | IF (e1, e2, e3) ->
            let b1 = find e1 var in
            let b2 = find e2 var in
            let b3 = find e3 var in
            b1 || b2 || b3
    | LET (x, e1, e2) ->
            let b1 = find e1 var in
            let b2 = find e2 var in
            b1 || b2
    | LETREC (f, x, e1, e2) ->
            let b1 = find e1 var in
            let b2 = find e2 var in
            b1 || b2
    | PROC (x, e) ->
            let b = find e var in b
    | CALL (e1, e2) ->
            let b1 = find e1 var in
            let b2 = find e2 var in
            b1 || b2

let rec solve: exp -> var -> exp -> exp
= fun exp1 var exp2 ->
    match exp1 with
    | CONST n -> CONST n
    | VAR x -> 
            if (find exp1 var) then exp2
            else exp1
    | ADD (e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> ADD (solve e1 var exp2, solve e2 var exp2)
            | (true, false) -> ADD (solve e1 var exp2, e2)
            | (false, true) -> ADD (e1, solve e2 var exp2)
            | (false, false) -> ADD (e1, e2))
    | SUB (e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> SUB (solve e1 var exp2, solve e2 var exp2)
            | (true, false) -> SUB (solve e1 var exp2, e2)
            | (false, true) -> SUB (e1, solve e2 var exp2)
            | (false, false) -> SUB (e1, e2))
    | MUL (e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> MUL (solve e1 var exp2, solve e2 var exp2)
            | (true, false) -> MUL (solve e1 var exp2, e2)
            | (false, true) -> MUL (e1, solve e2 var exp2)
            | (false, false) -> MUL (e1, e2))
    | DIV (e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> DIV (solve e1 var exp2, solve e2 var exp2)
            | (true, false) -> DIV (solve e1 var exp2, e2)
            | (false, true) -> DIV (e1, solve e2 var exp2)
            | (false, false) -> DIV (e1, e2))
    | READ -> exp1
    | ISZERO e ->
            if (find e var) then ISZERO (solve e var exp2)
            else ISZERO exp1
    | IF (e1, e2, e3) ->
            (match (find e1 var, find e2 var, find e3 var) with
            | (true, true, true) -> 
                    let ee1 = solve e1 var exp2 in
                    let ee2 = solve e2 var exp2 in
                    let ee3 = solve e3 var exp2 in
                    IF (ee1, ee2, ee3)
            | (true, true, false) ->
                    let ee1 = solve e1 var exp2 in
                    let ee2 = solve e2 var exp2 in
                    IF (ee1, ee2, e3)
            | (true, false, true) ->
                    let ee1 = solve e1 var exp2 in
                    let ee3 = solve e3 var exp2 in
                    IF (ee1, e2, ee3)
            | (true, false, false) ->
                    let ee1 = solve e1 var exp2 in
                    IF (ee1, e2, e3)
            | (false, true, true) ->
                    let ee2 = solve e2 var exp2 in
                    let ee3 = solve e3 var exp2 in
                    IF (e1, ee2, ee3)
            | (false, true, false) ->
                    let ee2 = solve e2 var exp2 in
                    IF (e1, ee2, e3)
            | (false, false, true) ->
                    let ee3 = solve e3 var exp2 in
                    IF (e1, e2, ee3)
            | (false, false, false) ->
                    IF (e1, e2, e3))
    | LET (x, e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> LET (x, solve e1 var exp2, solve e2 var exp2)
            | (true, false) -> LET (x, solve e1 var exp2, e2)
            | (false, true) -> LET (x, e1, solve e2 var exp2)
            | (false, false) -> LET (x, e1, e2))
    | LETREC (f, x, e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> 
                    let ee1 = solve e1 var exp2 in
                    let ee2 = solve e2 var exp2 in
                    LETREC (f, x, ee1, ee2)
            | (true, false) ->
                    let ee1 = solve e1 var exp2 in
                    LETREC (f, x, ee1, e2)
            | (false, true) ->
                    let ee2 = solve e2 var exp2 in
                    LETREC (f, x, e1, ee2)
            | (false, false) ->
                    LETREC (f, x, e1, e2))
    | PROC (x, e) ->
            if (find e var) then PROC (x, solve e var exp2)
            else PROC (x, e)
    | CALL (e1, e2) ->
            (match (find e1 var, find e2 var) with
            | (true, true) -> CALL (solve e1 var exp2, solve e2 var exp2)
            | (true, false) -> CALL (solve e1 var exp2, e2)
            | (false, true) -> CALL (e1, solve e2 var exp2)
            | (false, false) -> CALL (e1, e2))

let rec expand : exp -> exp 
= fun exp -> 
    match exp with
    | LET (v, e1, e2) ->
            if (find e2 v) then solve e2 v e1
            else exp
    | LETREC (f, v, e1, e2) ->
            (match (find e2 f, find e2 v) with
            | (true, true) -> 
                    let fine1 = solve e2 f e1 in
                    let fine2 = solve fine1 v e1 in
                    fine2
            | (true, false) ->
                    let fine1 = solve e2 f e1 in
                    fine1
            | (false, true) ->
                    let fine2 = solve e2 v e1 in
                    fine2
            | (false, false) ->
                    exp)
    | _ -> exp



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec listcheck: var -> var list -> bool
= fun var varl ->
    match varl with
    | [] -> false
    | hd :: tl -> if var = hd then true 
                  else listcheck var tl

let rec lamfind: lambda -> var list -> bool
= fun lam varl ->
    match lam with
    | V v -> listcheck v varl
    | P (v, lm) ->
            let vl = varl @ (v :: []) in
            let film = lamfind lm vl in
            film
    | C (lm1, lm2) ->
            let film1 = lamfind lm1 varl in
            let film2 = lamfind lm2 varl in
            film1 && film2

let check : lambda -> bool
= fun lam ->
    match lam with
    | V v -> false
    | P (v, lm) ->
            let chlm = lamfind lm (v :: []) in chlm
    | C (lm1, lm2) -> false
            

