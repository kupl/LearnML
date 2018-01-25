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
  = fun (exp, var) ->
  match exp with
  | Const i -> Const 0
  | Var i -> if i = var then Const 1 else Const 0
  | Power(i, n) -> if i = var then Times[Const n; Power(i, n-1)] else Const 0;
  | Times [] -> Const 0
  | Times(m::n) -> Sum[Times(diff(m, var)::n); Times[m; diff(Times n, var)]]
  | Sum [] -> Const 0
  | Sum(m::[]) -> diff(m, var)
  | Sum(m::n) ->  Sum[diff(m, var); diff(Sum n, var)]
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

  let rec plus : branch -> int
  = fun p ->
  match p with
  | SimpleBranch(m, n) -> n
  | CompoundBranch(m, (n, o)) -> (plus n) + (plus o);;

  let rec mul : branch -> int
  = fun mo ->
  match mo with
  | SimpleBranch(a, b) -> a * (plus mo)
  | CompoundBranch(a, (b, c)) -> a * (plus mo);;

  let rec balanced : mobile -> bool
  = fun mob ->
  let rec bal : branch -> bool
  = fun br ->
  match br with
  | SimpleBranch(l, w) -> true
  | CompoundBranch(l, (w1, w2)) -> if (mul w1) = (mul w2) then (bal w1) && (bal w2)
  else false
  in
  match mob with
  | (a, b) -> ((bal a) && (bal b)) && ((mul a) = (mul b))
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

  let rec calculator : exp -> int
    = fun exp ->
  match exp with
  | X -> raise NotImplemented
  | INT num -> num
  | ADD(m, n) -> (calculator m) + (calculator n)
  | SUB(m, n) -> (calculator m) - (calculator n)
  | MUL(m, n) -> (calculator m) * (calculator n)
  | DIV(m, n) -> (calculator m) / (calculator n)
  | SIGMA(m, n, p) ->
  if (calculator m) > (calculator n) then 0
  else if (calculator m) = (calculator n) then
  begin
   let rec cal : exp*exp -> int
    = fun(ex, i) ->
    match ex with
    | X -> cal(i, i)
    | INT num -> num
    | ADD(a, b) -> cal(a, i) + cal(b, i)
    | SUB(a, b) -> cal(a, i) - cal(b, i)
    | MUL(a, b) -> cal(a, i) * cal(b, i)
    | DIV(a, b) -> cal(a, i) / cal(b, i)
    | SIGMA(a, b, c) -> calculator c
  in
  cal(p, m)
  end
  else calculator(SIGMA(ADD(m, INT 1), n, p)) + calculator(SIGMA(m, m, p))
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

  let rec check : exp -> bool
  = fun exp ->
    let rec ch :  exp * var list -> bool
    = fun (r, lst) ->
    match r with
    | V a ->
    begin
    match lst with
    | [] -> false
    | m::n -> if m = a then true else ch(r, n)
    end
    | P(a, b) -> ch(b, lst@[a])
    | C(a, b) -> ch(a, lst) && ch(b, lst)
  in ch(exp, [])
end;;
