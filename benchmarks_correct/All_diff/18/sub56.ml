type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
    | Const a -> Const 0
    | Var a -> if a=x then Const 1 else Const 0 
    | Power (a,b)-> if a=x then Times [Const b; Power (a,b-1)] else Times [Power(a,b);Const 0]
    | Times lst->
      begin
      match lst with
        []->Const 0
        |hd::tl-> Sum [Times ((diff (hd,x))::tl); Times [hd; diff (Times tl,x)]]
      end
    | Sum lst->
      begin
      match lst with
        []->Const 0
        |hd::tl-> Sum[diff (hd,x); diff (Sum tl,x)]
      end
      ;;

        