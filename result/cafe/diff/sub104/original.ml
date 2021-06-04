type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff (f : aexp * string) : aexp =
  match f with
  | exp, str -> (
      let e : aexp = exp in

      let s0 : string = str in

      let rec diffsum (l : aexp list) (str : string) : aexp list =
        match l with
        | [] -> []
        | hd :: tl ->
            if hd = Const 0 then diffsum tl s0
            else diff (hd, s0) :: diffsum tl s0
      in

      match e with
      | Const n -> Const 0
      | Var s1 -> if s1 = s0 then Const 1 else Var s1
      | Power (s2, n) ->
          if s2 = s0 then Times [ Const n; Power (s2, n - 1) ] else Power (s2, n)
      | Sum l -> Sum (diffsum l s0)
      | Times l ->
          let rec swap (e : aexp) (l0 : aexp list) : aexp list =
            match l0 with
            | [] -> []
            | hd :: tl -> if hd = e then diff (e, s0) :: tl else hd :: swap e tl
          in

          let rec difftimes (l1 : aexp list) : aexp list =
            match l1 with
            | [] -> []
            | hd :: tl -> Times (swap hd l) :: difftimes tl
          in
          Sum (difftimes l) )
