type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun f ->
  match f with
  | exp, str -> (
      let e = exp in

      let s0 = str in

      let rec diffsum : aexp list -> string -> aexp list =
       fun l str ->
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
          let rec swap : aexp -> aexp list -> aexp list =
           fun e l0 ->
            match l0 with
            | [] -> []
            | hd :: tl -> if hd = e then diff (e, s0) :: tl else hd :: swap e tl
          in

          let rec difftimes : aexp list -> aexp list =
           fun l1 ->
            match l1 with
            | [] -> []
            | hd :: tl -> Times (swap hd l) :: difftimes tl
          in
          Sum (difftimes l) )
