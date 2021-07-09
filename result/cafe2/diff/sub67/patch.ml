type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const k -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, n) ->
      if str != x then Const 0 else Times [ Const n; Power (str, n - 1) ]
  | Times lst -> (
      if List.mem (Const 0) lst then Const 0
      else
        match lst with
        | [] -> raise InvalidArgument
        | [ hd ] -> diff (hd, x)
        | hd :: tl -> (
            match hd with
            | Const 1 -> diff (Times tl, x)
            | Const k -> Times [ hd; diff (Times tl, x) ]
            | _ ->
                Sum
                  [
                    Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ];
                  ] ) )
  | Sum lst -> (
      match lst with
      | [] -> raise InvalidArgument
      | [ hd ] -> diff (hd, x)
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
