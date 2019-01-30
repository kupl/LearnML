type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
  | INT a -> a
  | ADD (a,b) -> calculator a + calculator b
  | SUB (a,b) -> calculator a - calculator b
  | MUL (a,b) -> calculator a * calculator b
  | DIV (a,b) -> calculator a / calculator b
  | SIGMA (a,b,ex) -> let rec sigfunc : int -> int -> exp -> int
  = fun a b ex -> let rec xcal : exp -> int -> int
  = fun l x -> match l with
    | X -> x
    | INT a -> a
    | ADD (a,b) -> xcal a x + xcal b x
    | SUB (a,b) -> xcal a x - xcal b x
    | MUL (a,b) -> xcal a x * xcal b x
    | DIV (a,b) -> xcal a x / xcal b x
    in if a>b then 0 else (xcal ex a)+(sigfunc (a+1) b ex)
    in sigfunc (calculator a) (calculator b) ex;;
    