(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
 
let rec diff : aexp * string -> aexp
=fun (exp,var) ->

match exp with
Const (a) -> Const 0 |
Var (s) -> if s=var then Const 1 else Const 0 |
Power (s,a) -> if a=1 then Const 1 else Times [Const a; Power (s,a-1)] |
Times lst -> (match lst with [] -> Const 0 |
  h::t -> if t = [] then Sum [Times [(diff (h,var)); Const 1]; Times [h;(diff (Times t,var))]]
  else Sum [Times [(diff (h,var)); Times t]; Times [h;(diff (Times t,var))]]) |
Sum lst2 -> match lst2 with [] -> Const 0 | h::t -> Sum [(diff (h,var));(diff (Sum t,var))]
end;; 

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec help1 temp =
    match temp with
    SimpleBranch (l,w) -> w |
    CompoundBranch (l,m) ->
        (match m with
        (a,b) -> (help1 a) + (help1 b));;

let rec help2 temp =
    match temp with
    SimpleBranch (l,w) -> l*w |
    CompoundBranch (l,m) ->
        (match m with
        (a,b) -> if (help2 a)=(help2 b) then (((help1 a)+(help1 b))*l) else (-1));;

let balanced : mobile -> bool
=fun (lb,rb) ->
     if ((help2 lb)=(help2 rb)) then true else false
end;;

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp ->
    let rec oper t =
      (match t with
      X -> raise NotImplemented  |
      INT n1 -> n1 |
      ADD (n1, n2) -> (oper n1) + (oper n2) |
      SUB (n1, n2) -> (oper n1) - (oper n2) |
      MUL (n1, n2) -> (oper n1) * (oper n2) |
      DIV (n1, n2) -> (oper n1) / (oper n2) |
      SIGMA (n1, n2, n3) ->
      let rec cal k =
         (match k with
         X -> (fun x->x) |
         INT i -> (fun x->i) |
         ADD (p,q) -> (fun x->((cal p) x)+((cal q) x)) |
         SUB (p,q) -> (fun x->((cal p) x)-((cal q) x)) |
         MUL (p,q) -> (fun x->((cal p) x)*((cal q) x)) |
         DIV (p,q) -> (fun x->((cal p) x)/((cal q) x)) |
         SIGMA (p,q,r) -> raise (Failure "impossible"))
       in
       let rec sigma v1 v2 =
         if v1>v2 then 0
         else if v1=v2 then ((cal n3) v1)
         else (((cal n3) v1) + (sigma (v1+1) v2))
       in (sigma (oper n1) (oper n2)))
    in
    match exp with
    X -> 0 |
    INT a -> a |
    ADD (a,b) -> (oper (ADD (a,b))) |
    SUB (a,b) -> (oper (SUB (a,b))) |
    MUL (a,b) -> (oper (MUL (a,b))) |
    DIV (a,b) -> (oper (DIV (a,b))) |
    SIGMA (a,b,c) -> (oper (SIGMA (a,b,c)));;
end;;

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec evaluate temp1 temp2 =
    match temp1 with
    V a ->
        let rec ck l1 st =
          match l1 with
          [] -> false |
          h::t -> if h=st then true else (ck t st) in
        if (ck temp2 a) then true else false |
    P (v, e1) -> if (evaluate e1 (temp2@[v])) then true else false |
    C (e1, e2) -> if (evaluate e1 temp2 && evaluate e2 temp2) then true else false;;

let check : exp -> bool
=fun exp ->
      evaluate exp []
end;;

