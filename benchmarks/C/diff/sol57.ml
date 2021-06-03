type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

exception InvalidArgument

let rec diff (aexp, str) =
  let rec strip aexp =
    match aexp with
    | Times [] -> Const 1
    | Times ((Const 0)::tl) -> Const 0
    | Times [hd;(Const 0)] -> Const 0
    | Times ((Const 1)::tl) -> strip (Times tl)
    | Times [hd;(Const 1)] -> hd
    | Times (hd::[]) -> hd
    | Sum [] -> Const 0
    | Sum (hd::[]) -> hd
    | Sum [hd;(Const 0)] -> hd
    | Sum ((Const 0)::tl) -> strip (Sum tl)
    | _ -> aexp in

  match aexp with
  | Const (i) -> Const 0
  | Var (s) ->
      if (s=str) then Const 1
      else Const 0
  | Power (s, i) ->
      if s=str then strip (Times [Const i; Power (s, i-1)])
      else Const 0
  | Times [] -> raise InvalidArgument
  | Times (hd::tl) -> strip (Sum [strip (Times ((diff (hd, str))::tl)); strip
  (Times [hd; diff (strip (Times  tl), str)])])
  | Sum [] -> raise InvalidArgument
  | Sum (a::b) -> (strip (Sum ([(diff (a, str));(diff ((strip (Sum b)), str))])))
