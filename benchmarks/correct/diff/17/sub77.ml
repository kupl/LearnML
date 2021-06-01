(*problem4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (e,x) -> match e with
  Const(a)->Const(0)
  |Var(a)->if(a=x) then Const(1) else Const(0)
  |Power(a,b)->if(a=x) then
    (if(b<>0) then Times[Const b;Power(a,(b-1))] else Const 0)
  else Const 0
  |Times a->(match a with
    hd::tl->if(tl<>[])
      then Sum [Times[(diff (hd,x));Times tl];Times[hd;diff (Times tl,x);]]
      else diff (hd,x)
    |[]->Const 0)
  |Sum a->(match a with
    hd::tl-> if(tl<>[])then Sum[diff (hd,x);diff (Sum tl,x);]else (diff (hd,x))
    |[]->Const 0);;