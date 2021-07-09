type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var k -> if k = var then Const 1 else Var k
  | Power (s, n) ->
      if s = var then
        if n = 0 then Const 0 else Times [ Power (s, n - 1); Const n ]
      else Const 0
  | Times li ->
      let rec timesfun (exp : aexp list) (var : string) : aexp list =
        match exp with
        | [] -> []
        | hd :: tl -> (
            match hd with
            | Const n -> [ Const n ] @ timesfun tl var
            | Var k -> [ diff (hd, var) ] @ timesfun tl var
            | Power (s, n) ->
                if s = var then
                  if n = 0 then [ Const 0 ] @ timesfun tl var
                  else [ Times [ Power (s, n - 1); Const n ] ] @ timesfun tl var
                else [ Const 1 ] @ timesfun tl var
            | Times li -> [ diff (hd, var) ] @ timesfun tl var
            | Sum li -> [ diff (hd, var) ] @ timesfun tl var )
      in
      Times (timesfun li var)
  | Sum li ->
      let rec sumfun (exp : aexp list) : aexp list =
        match exp with [] -> [] | hd :: tl -> [ diff (hd, var) ] @ sumfun tl
      in
      Sum (sumfun li)
