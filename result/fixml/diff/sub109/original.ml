type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, var) ->
  match (exp, var) with
  | Const n, str -> Const 0
  | Var x, str -> if str = x then Const 1 else Const 0
  | Power (x, n), str ->
      if str = x then Times [ Const n; Power (x, n - 1) ] else Const 0
  | Times l, str -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          if diff (hd, str) = Const 0 then
            let lst = [ hd ] @ [ diff (Times tl, str) ] in
            Times lst
          else
            let lst = [ diff (hd, str) ] @ tl in
            Times lst )
  | Sum l, str -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          let lst = [ diff (hd, str) ] @ [ diff (Sum tl, str) ] in
          Sum lst )
  | _ -> raise Failure "wrong expression"
