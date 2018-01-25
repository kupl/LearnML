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
|Const(a) -> Const(0)
|Var(a)->       if a=var then Const(1) else Const(0)
|Power(a,b)->   if a=var then Times[Const(b); Power(a,(b-1))] else Const(0)
|Times(lst) ->
       ( match lst with
        |hd::tl -> if tl =[] then diff(hd,var)
                else  Sum[Times[diff(hd,var); Times tl ]; Times[hd; diff(Times tl, var)]]
        |[]->Const(0)
        )
|Sum(lst)->
       ( match lst with
        |hd::tl -> if tl = [] then diff(hd,var)
                else  Sum[diff(hd, var); diff(Sum tl, var)]
        |[]->Const(0)
        )
end
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

let rec cnt : branch -> int = fun m->
match m with
|SimpleBranch(a,b)-> b
|CompoundBranch(a,b)-> match b with
        |(x,y)->cnt(x)+cnt(y)

let rec cal :branch -> int = fun m ->
match m with
|SimpleBranch(a,b)-> a*b
|CompoundBranch(a,b)-> match b with
        |(x,y)->if balanced(x,y) then  a*cnt(x)+a*cnt(y)
                else (-1)
and  balanced : mobile -> bool
  = fun mob ->
match mob with
|(a,b) -> if cal(a)=(-1) then false
        else if cal(b)=(-1) then false
        else  cal(a)=cal(b)
end
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

let rec sighelp : int*exp->int = fun(key,exp)->
match exp with
|X->key
|INT(a)->a
|ADD(a,b)->sighelp(key,a)+sighelp(key,b)
|SUB(a,b)->sighelp(key,a)-sighelp(key,b)
|MUL(a,b)->sighelp(key,a)*sighelp(key,b)
|DIV(a,b)->sighelp(key,a)/sighelp(key,b)
|SIGMA(a,b,c)->if calculator(a)>calculator(b) then 0
                else sighelp(calculator(a),c)+calculator(SIGMA(ADD(a,INT(1)),b,c))
and  calculator : exp -> int
  = fun exp ->
match exp with
|X-> raise NotImplemented
|INT(a)->a
|ADD(a,b)->calculator(a)+calculator(b)
|SUB(a,b)->calculator(a)-calculator(b)
|MUL(a,b)->calculator(a)*calculator(b)
|DIV(a,b)->calculator(a)/calculator(b)
|SIGMA(a,b,c)->if calculator(a)>calculator(b) then 0
                else sighelp(calculator(a),c)+calculator(SIGMA(ADD(a,INT(1)),b,c))
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec searchlist : var*var list -> bool
=fun(v,lst)->
match lst with
|hd::tl -> if hd=v then true else searchlist(v,tl)
|[]-> false

let rec search : exp*var list->bool
=fun(exp,lst)->
match exp with
|P(a,b)-> search(b,a::lst)
|V(a)->searchlist(a,lst)
|C(a,b)->if search(b,lst) && search(a,lst) then true
        else false

  let rec check : exp -> bool
  = fun exp ->
let lst = [] in
match exp with
|P(a,b) -> search(b,a::lst)
|V(a) -> false
|C(a,b) -> if check(a)=true && check(b)=true then true else false

end