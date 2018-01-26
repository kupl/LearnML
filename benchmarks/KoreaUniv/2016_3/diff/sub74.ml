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

  let rec diff: aexp * string -> aexp
  = fun (exp, var) ->
  match exp, var with
  |(Const n,str)-> Const 0
  |(Var x,str)->
   (if str=x then Const 1
     else Const 0)
  |(Power(x,n),str)->
   (if str=x then Times [Const n;Power(x,n-1)]
   else Const 0)
  |(Times l,str)->
    (match l with
    |[]->Const 0
    |hd::tl->if diff (hd,str) =Const 0 then
    let lst= [hd]@ [diff(Times tl,str)] in (Times lst)
    else let lst=[diff (hd,str)]@ tl in (Times lst) )
  |(Sum l,str)->
    (match l with
    |[]->Const 0
    |hd::tl->let lst=[diff(hd,str)]@[diff((Sum tl),str)] in Sum lst)
  |_ ->raise (Failure "wrong expression")

   ;;

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

  let rec balanced : mobile -> bool
  = fun mob->
match mob with
| SimpleBranch(ll,lw),SimpleBranch(rl,rw)->
if ll*lw=rl*rw then true
else false
| SimpleBranch(ll,lw),CompoundBranch(rl,rw)->
if balanced rw=true then
(if ll*lw=rl*(mob_weight rw) then true
else false)
else false
| CompoundBranch(ll,lw),SimpleBranch(rl,rw)->
if balanced lw=true then
(if ll*(mob_weight lw)=rl*rw then true
else false)
else false
| CompoundBranch(ll,lw),CompoundBranch(rl,rw)->
if (balanced lw=true)&&(balanced rw=true) then
(if ll*(mob_weight lw)=rl*(mob_weight rw) then true
else false)
else false
|_ ->raise (Failure "wrong expression")

and mob_weight :mobile->int
=fun mob->
match mob with
| SimpleBranch(ll,lw),SimpleBranch(rl,rw)->lw+rw
| SimpleBranch(ll,lw),CompoundBranch(rl,rw)->lw+(mob_weight rw)
| CompoundBranch(ll,lw),SimpleBranch(rl,rw)->(mob_weight lw)+rw
| CompoundBranch(ll,lw),CompoundBranch(rl,rw)->(mob_weight lw)+(mob_weight rw);;

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


let rec calculator : exp -> int
= fun exp ->
   match exp with
   | X -> 1
   | INT n -> n
   | ADD (e1, e2) -> calculator e1 + calculator e2
   | SUB (e1, e2) -> calculator e1 - calculator e2
   | MUL (e1, e2) -> calculator e1 * calculator e2
   | DIV (e1, e2) -> calculator e1 / calculator e2
   | SIGMA (e1, e2, e3) ->
   if (calculator e1)=(calculator e2) then
   calculator e3
   else let i=calculator e1 in
   let k= calculator e2 in
   calculator e3 + calculator(SIGMA(INT (i+1),INT k,e3))

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

  let rec compre1 :var*exp -> bool
  =fun (exp1,exp2)->
  match exp1,exp2 with
  | var,V vari->
   if var=vari then true
   else false
  | (var,P(vari,expi))->
   if (var=vari) &&compre1(var,expi)then true
   else false
  | (var,C(exp1,exp2))->
  compre1(var,exp1)||compre1(var,exp2)

  let rec compre2 :exp*exp -> bool
  =fun (exp1,exp2)->
  match exp1,exp2 with
  | V vari,V varj->
  if  vari=varj then true
  else false
  | V vari,P(varj,expj)->
  if (vari=varj)&&compre1(vari,expj)
  | V vari,C(expji,expjj)->
  | P(vari,expi),P(varj,expj)->
  | P(vari,expi),C(expji,expjj)->
  | C(expii,expij),C(expji,expjj)->



  let check : exp -> bool
  = fun exp->
match exp with
| V var-> true
| P(var,exp)-> compre1(var,exp)
| C(exp1,exp2)-> compre2(exp1,exp2)



end
