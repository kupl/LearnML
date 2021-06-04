type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const i -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, i) ->
      if a = x then
        match i with
        | 1 -> Const i
        | 2 -> Times [ Const i; Var a ]
        | _ -> Times [ Const i; Power (a, i - 1) ]
      else Const 0
  | Times lst ->
      if
        let rec search (lst : aexp list) (x : string) : int =
          match lst with
          | hd :: tl -> (
              match hd with
              | Power (a, i) -> 1 + search tl x
              | Var a -> if a = x then 1 else 0 + search tl x
              | _ -> search tl x )
          | _ -> 0
        in
        search lst x = 0
      then Const 0
      else
        Times
          (let rec yes (lst : aexp list) (x : string) : aexp list =
             match lst with
             | hd :: tl -> (
                 match hd with
                 | Const i -> Const i :: yes tl x
                 | Var a -> if a = x then diff (hd, x) else hd :: yes tl x
                 | Power (a, i) ->
                     if a = x then diff (hd, x) else hd :: yes tl x
                 | _ -> diff (hd, x) :: yes tl x )
             | _ -> []
           in
           yes lst x)
  | Sum lst ->
      Sum
        (let rec sumdiff (lst : aexp list) (x : string) : aexp list =
           match lst with hd :: tl -> diff (hd, x) :: sumdiff tl x | [] -> []
         in
         sumdiff lst x)
