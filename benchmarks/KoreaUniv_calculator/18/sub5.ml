type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec iterator =
  fun f a b -> if a > b then 0 else f a + iterator f (a+1) b;;
  
let rec makeFun =
  fun exp -> match exp with
    | INT i -> fun x -> i
    | X -> fun x->x
    | SIGMA (e1, e2, e3) -> fun x->iterator (makeFun e3) (makeFun e1 x) (makeFun e2 x)
    | ADD (e1, e2) -> fun x -> makeFun e1 x + makeFun e2 x
    | SUB (e1, e2) -> fun x -> makeFun e1 x - makeFun e2 x
    | MUL (e1, e2) -> fun x -> makeFun e1 x * makeFun e2 x
    | DIV (e1, e2) -> fun x -> makeFun e1 x / makeFun e2 x;;
 
let rec calculator : exp -> int
= fun exp -> match exp with
  | INT i -> i
  | ADD (e1, e2) -> calculator e1 + calculator e2
  | SUB (e1, e2) -> calculator e1 - calculator e2
  | MUL (e1, e2) -> calculator e1 * calculator e2
  | DIV (e1, e2) -> calculator e1 / calculator e2
  | SIGMA (e1, e2, e3) -> iterator (makeFun e3) (calculator e1) (calculator e2);;
  