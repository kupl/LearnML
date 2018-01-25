(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
     [] -> []
    |hd::tl ->
      if pred hd then hd::filter pred tl
    else filter pred tl ;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a with
     [] -> b
    |hd :: tl -> hd :: zipper (b, tl) ;;

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  let compose ((f, g) : (int -> int) * (int -> int)) (x : int) : int = f (g x) in
  if n = 0 then f
  else if n > 0 then compose(f, iter(n-1, f) )
  else f;;


(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  let rec diffList : aexp list -> aexp list
    = fun al -> 
    match al with
    | [] -> []
    | [e] -> diff(e,x)::[]
    | hd::tl -> diff(hd,x)::diffList(tl)
  in
  match aexp with
  | Const c -> Const 0
  | Var v -> Var v
  | Power (v, c) -> 
      if v = x then Times [Const c; Power (v, c-1)]
    else Const 0
  | Times l ->
      (match l with
      | [] -> Times []
      | hd::tl -> Times (diffList l))
  | Sum l -> 
      (match l with
      | [] -> Sum []
      | hd::tl -> Sum (diffList l));;

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
=fun e -> 
  let rec eval exp = X in
  match e with
    X -> X 
  | INT v -> v
  | ADD (e,e') -> calculator e + calculator e'
  | SUB (e,e') -> calculator e - calculator e'
  | MUL (e,e') -> calculator e * calculator e'
  | DIV (e,e') -> calculator e / calculator e'
  | SIGMA (e,e',e'') -> 
        if e = e' then calculator e''
      else calculator e'' + calculator (SIGMA(ADD(e , INT 1), e', e''));;
