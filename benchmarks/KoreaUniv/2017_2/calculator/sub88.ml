(*5*)

  type exp =
     X
    | INT of int
    | ADD of exp*exp
    | SUB of exp*exp
    | MUL of exp*exp
    | DIV of exp*exp
    | SIGMA of exp*exp*exp  
(*
  type env = (var*value) list;;
  let empty_env=[];;
  let extend_env (x,v) e = (x,v) ::e;;
  let rec apply_env x e =
    match e with
    | []-> raise (Failure("variable not fount"))
    | (y,v) :: tl -> if x=y then v else apply_env x tl;;
*)
  let rec calculatortemp : exp*int ->exp =fun (e, t) ->
  match e with
  | X->INT t
  | INT n-> INT n
  | ADD(e1,e2) -> ADD(calculatortemp(e1,t),calculatortemp(e2,t))
  | SUB(e1,e2) -> SUB(calculatortemp(e1,t),calculatortemp(e2,t))
  | MUL(e1,e2) -> MUL(calculatortemp(e1,t),calculatortemp(e2,t))
  | DIV(e1,e2) -> DIV(calculatortemp(e1,t),calculatortemp(e2,t))
  | SIGMA(e1,e2,e3)->calculatortemp(e3,t);;

  let rec calculator : exp -> int = fun e->
  (* let rec eval env = function
  match env with
  |Var x -> List.assoc x env
  *)
   match e with
  |INT k -> k
  |ADD (e1,e2) -> 
    let v1 = calculator e1 in
    let v2 = calculator e2  in
      v1+v2
  |SUB (e1,e2) ->
    let v1 = calculator e1 in
    let v2 = calculator e2  in
      v1-v2
  |MUL (e1,e2) -> 
    let v1 = calculator e1 in
    let v2 = calculator e2 in
      v1*v2
  |DIV (e1,e2) -> 
    let v1 = calculator e1 in
    let v2 = calculator e2 in
      v1/v2
  |SIGMA (e1,e2,e3) ->
    let v1 = calculator e1 in
    let v2 = calculator e2 in
    let v3 = calculator(calculatortemp(SIGMA(e1,e2,e3),calculator (e1))) in
    if v1>v2 then 0 else v3 + calculator(SIGMA(ADD(e1, INT 1),e2,e3));;
