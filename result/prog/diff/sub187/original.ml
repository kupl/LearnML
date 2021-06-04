type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec isExist (exp : aexp) (x : string) : bool =
  match exp with
  | Const n -> false
  | Var v -> if v = x then true else false
  | Power (v, n) -> if v = x then true else false
  | Times t ->
      let rec reTime (lst : aexp list) : bool =
        match lst with [] -> false | hd :: tl -> isExist hd x || reTime tl
      in
      reTime t
  | Sum s ->
      let rec reSum (lst : aexp list) : bool =
        match lst with [] -> false | hd :: tl -> isExist hd x || reSum tl
      in
      reSum s


let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var v -> if v = x then Const 1 else Var v
  | Power (v, n) ->
      if v = x then Times [ Const n; Power (v, n - 1) ] else Power (v, n)
  | Times t ->
      if not (isExist (Times t) x) then Const 0
      else
        let rec reTime (lst : aexp list) : aexp list =
          match lst with
          | [] -> []
          | hd :: tl -> (
              match hd with
              | Const n -> hd :: reTime tl
              | _ -> diff (hd, x) :: reTime tl )
        in
        Times (reTime t)
  | Sum s ->
      if not (isExist (Sum s) x) then Const 0
      else
        let rec reSum (lst : aexp list) : aexp list =
          match lst with [] -> [] | hd :: tl -> diff (hd, x) :: reSum tl
        in
        Sum (reSum s)


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "y")


let (_ : aexp) = diff (Sum [ Times [ Var "y"; Var "x" ]; Const 2 ], "y")
