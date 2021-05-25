type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    Const n -> Const 0
    |Var str -> if str = x then Const 1 else Const 0
    |Power (str,n) -> if str =x then Times[Const n;Power(str,n-1)] else Const 0
    |Times aexp -> diff_time (aexp,x)
    |Sum aexp -> diff_sum (aexp,x)
    
and diff_time (expl,x) = 
  match expl with
    []-> Const 1
    |[hd] -> diff(hd,x)
    |hd::tl -> Sum [Times(diff(hd,x)::tl); Times [hd;diff(Times tl,x)]]

and diff_sum (expl,x) = 
  match expl with
    []-> Const 0
    |[hd] -> diff (hd,x)
    |hd::tl -> Sum [diff(hd,x);diff(Sum tl,x)] ;;

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1],"x");;