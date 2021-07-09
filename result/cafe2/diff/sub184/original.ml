type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const i -> Const 0
  | Var s -> if s = x then Const 1 else Var s
  | Power (s, i) ->
      if s = x then
        if i > 2 then Times [ Const i; Power (s, i - 1) ]
        else Times [ Const i; Var s ]
      else Power (s, i)
  | Times al ->
      let rec timeiter (lst : aexp list) : aexp list =
        match lst with
        | [] -> []
        | hd :: tl ->
            (match hd with Const i1 -> Const i1 | _ -> diff (hd, x))
            :: timeiter tl
      in
      Times (timeiter al)
  | Sum al ->
      let rec sumeval (lst : aexp list) : aexp list =
        match lst with
        | [] -> []
        | hd :: tl -> if hd = Const 0 then sumeval tl else hd :: sumeval tl
      in

      let rec sumiter (lst : aexp list) : aexp list =
        match lst with [] -> [] | hd :: tl -> diff (hd, x) :: sumiter tl
      in
      Sum (sumeval (sumiter al))


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")