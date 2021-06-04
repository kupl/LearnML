type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec simplify a =
  match a with
  | Power (str, n) -> if n = 0 then Const 1 else if n = 1 then Var str else a
  | Times hd :: tl ->
      if tl = [] then simplify hd
      else if simplify hd = Const 0 || simplify (Times tl) = Const 0 then
        Const 0
      else if simplify hd = Const 1 then simplify (Times tl)
      else if simplify (Times tl) = Const 1 then simplify hd
      else Times ([ simplify hd ] @ [ simplify (Times tl) ])
  | Sum hd :: tl ->
      if tl = [] then simplify hd
      else if simplify hd = Const 0 then simplify (Sum tl)
      else if simplify (Sum tl) = Const 0 then simplify hd
      else Sum ([ simplify hd ] @ [ simplify (Times tl) ])
  | _ -> a


let rec diff (a, s) =
  match a with
  | Const _ -> Const 0
  | Var str -> if s = str then Const 1 else Const 0
  | Power (str, n) ->
      if s = str then simplify (Times ([ Const n ] @ [ Power (str, n - 1) ]))
      else Const 0
  | Times hd :: tl ->
      simplify
        (Sum
           ( [ Times ([ diff (hd, s) ] @ tl) ]
           @ [
               Times
                 ( [ hd ]
                 @ if tl = [] then [ Const 0 ] else [ diff (Times tl, s) ] );
             ] ))
  | Times [] -> raise InvalidArgument
  | Sum hd :: tl ->
      simplify
        (Sum
           ( [ diff (hd, s) ]
           @ if tl = [] then [ Const 0 ] else [ diff (Sum tl, s) ] ))
  | Sum [] -> raise InvalidArgument
