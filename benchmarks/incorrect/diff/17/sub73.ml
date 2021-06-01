(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
|Const n -> Const 0
|Var a -> if a=x then Const 1 else Const 0
|Power (a,n) -> if a=x && n=2 then Times[Const 2; Var a]
                else if a=x then Times[Const n; Power(a,n-1)]
                else Const 0
|Times l -> begin
  match l with
  |[] -> raise (Failure "unaccepted form")
  |[e] -> raise (Failure "unaccepted form")
  |[e1;e2] -> Sum [Times [diff (e1,x); e2]; Times [e1; diff (e2,x)]]
  |hd::tl -> Sum [Times [diff (hd,x); Times tl]; Times [hd; diff (Times tl,x)]]
  end
|Sum m -> begin
  match m with
  |[] -> raise (Failure "unaccepted form")
  |[e] -> raise (Failure "unaccepted form")
  |[e1;e2] -> Sum [diff (e1,x); diff(e2,x)]
  |hd::tl -> Sum [diff (hd,x); diff (Sum tl,x)]
  end
;;
