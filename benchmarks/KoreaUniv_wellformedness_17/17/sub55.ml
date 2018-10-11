
(*1*)
type exp =
  |CONST of int
  |VAR of var
  |ADD of exp*exp
  |SUB of exp*exp
  |MUL of exp*exp
  |DIV of exp*exp
  |ISZERO of exp
  |READ
  |IF of exp*exp*exp
  |LET of var*exp*exp
  |LETREC of var*var*exp*exp
  |PROC of var*exp
  |CALL of exp*exp
  and var = string

(*test cases*)
let pgm1 = LET ("X",CONST 1, VAR "X")
let pgm2 = LET ("f", PROC ("x", VAR "x"),
            IF(CALL(VAR"f", ISZERO (CONST 0)),
               CALL(VAR"f", CONST 11),
               CALL(VAR"f", CONST 22)))
let pgm3 = LET ("x", ADD( CONST 1, ISZERO (CONST 0)), CONST 2)

let rec detect : exp->string ->bool =fun exp x ->
 (match exp with
  |VAR v -> if x=v then true else false
  |CONST n -> false
  |ISZERO e -> detect e x
  |ADD (e1,e2) -> detect e1 x || detect e2 x 
  |SUB (e1,e2) -> detect e1 x || detect e2 x 
  |MUL (e1,e2) -> detect e1 x || detect e2 x 
  |DIV (e1,e2) -> detect e1 x || detect e2 x
  |READ -> false
  |IF (e1,e2,e3) -> detect e1 x || detect e2 x || detect e3 x 
  |LET (v,e1,e2) -> detect e1 x || detect e2 x
  |LETREC(v1,v2,e1,e2) -> detect e1 x|| detect e2 x
  |PROC (v,e) ->detect e x
  |CALL (e1,e2) ->detect e1 x || detect e2 x
)
let rec change : string -> exp -> exp = fun x exp1->
 (match exp1 with
 |VAR x ->exp1
 |CONST n -> CONST n
 |ISZERO e -> 
  let etemp = change x e in
  ISZERO etemp
 |ADD (e1,e2) -> 
  let e1temp = change x e1 in
  let e2temp = change x e2 in
  ADD (e1temp , e2temp )
 |SUB (e1,e2) -> 
  let e1temp = change x e1 in
  let e2temp = change x e2 in
  SUB (e1temp, e2temp)
 |MUL (e1,e2) ->
  let e1temp = change x e1 in
  let e2temp = change x e2 in
  MUL (e1temp,e2temp )
 |DIV (e1,e2) -> 
  let e1temp = change x e1 in
  let e2temp = change x e2 in
  DIV (e1temp,e2temp)
 |READ -> READ
 |IF (e1,e2,e3) -> 
  let e1temp = change x e1 in
  let e2temp = change x e2 in
  let e3temp = change x e3 in
 IF (e1temp,e2temp,e3temp)
 |LET (v,e1,e2) -> 
  let e1temp = change x e1 in
  let e2temp = change x e2 in
 LET(v,e1temp, e2temp)
 |LETREC(v1,v2,e1,e2) ->
  let e1temp = change x e1 in
  let e2temp = change x e2 in
 LETREC (v1,v2, e1temp,e2temp)
 |PROC (v,e) ->
  let etemp = change x e in
  PROC (v,etemp)
 |CALL (e1,e2) -> 
  let e1temp = change x e1 in
  let e2temp = change x e2 in
 CALL (e1temp, e2temp )
 )
 (*
 (match (VAR x) with 
  | VAR x -> e1
  | _ -> raise (Failure("Error!"))
  )
  *)
(*let (VAR x) = e1 in (VAR x)
*)
let rec expand : exp -> exp = fun exp ->
 (match exp with
  |CONST n -> CONST n
  |VAR x -> VAR x
  |ADD (e1,e2) -> 
   let e1temp =expand e1 in
   let e2temp = expand e2 
   in ADD (e1temp,e2temp)
  |SUB (e1,e2) -> 
   let e1temp = expand e1 in
   let e2temp = expand e2 in
   SUB (e1temp,e2temp)
  |MUL (e1,e2) -> 
   let e1temp = expand e1 in
   let e2temp = expand e2 in
   MUL (e1temp,e2temp)
  |DIV (e1,e2) -> 
   let e1temp = expand e1 in
   let e2temp = expand e2 in
   DIV (e1temp,e2temp)
  |ISZERO exp -> 
   let etemp = expand exp in
   ISZERO etemp
  |READ -> READ
  |IF (e1,e2,e3) ->
   let e1temp = expand e1 in
   let e2temp = expand e2 in
   let e3temp = expand e3 in
    IF (e1temp,e2temp,e3temp)
  |LET(var,e1,e2) -> 
   let e1temp = expand e1 in
   let e2temp = expand e2 in
   if(detect e2 var) then (change var e1temp)  else LET ( var, e1temp, e2temp)
  |LETREC (var1,var2,e1,e2) -> LETREC(var1,var2,e1,e2)
  |PROC (var, e) -> 
   let etemp = expand e in
    PROC ( var, etemp) 
  |CALL (e1,e2) ->
   let e1temp = expand e1 in
   let e2temp = expand e2 in
   CALL (e1temp,e2temp)
  (*
  |CALL (e1,e2)-> 
    (match e1 with
    |VAR v -> CALL ((PROC( v,VAR v)),e2)
    |_-> e2
    )
  *)
)

(*2*)
type lambda = V of var
             |P of var * lambda
             |C of lambda * lambda
             and var = string

 let rec check : lambda -> bool
 = fun lam ->
  let rec sub : lambda -> var list -> bool
   = fun lam v1 ->
    (match lam with
      | V v -> ( (List.exists (fun x->x =v) v1))
      | P (v,lam) -> ( sub lam (v :: v1))
      | C (lam1, lam2) -> if (sub lam1 v1) then (sub lam2 v1) else false) in sub lam []
