
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec listt1 : aexp list*string -> aexp list
  = fun (lst,var) -> match lst with
  | [] -> []
  | hd::tl -> diff(hd,var)::listt1(tl,var)

  and listt2 : aexp list*string*aexp list -> aexp list
  = fun (lst,var,temp) -> match lst with
  | [] -> []
  | hd::tl -> Times((temp@diff(hd,var)::tl))::listt2(tl,var,(temp@[hd]))

  and diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Sum(lst) -> Sum(listt1(lst,var))
  | Times(lst) -> Sum(listt2(lst,var,[]))
  | Power(v,n) -> if(var=v) then if n>2 then Times([Const n;Power(v,n-1)]) else if n=2 then Times([Const 2;Var v]) else if n=1 then Const 1 else Const 0
                  else Const(0)
  | Const(x) -> Const(0)
  | Var(v) -> if(var=v) then Const(1)
              else Const(0)