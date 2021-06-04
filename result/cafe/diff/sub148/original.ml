type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((e : aexp), (x : string)) : aexp =
  let rec d (exp : aexp) (isTimesWithX : bool) : aexp =
    match exp with
    | Sum l ->
        let rec iter (tmp : aexp list) : aexp list =
          match tmp with [] -> [] | hd :: tl -> d hd false :: iter tl
        in
        Sum (iter l)
    | Times l ->
        let tmpBool : bool =
          let rec iter (tmp : aexp list) : bool =
            match tmp with
            | [] -> false
            | hd :: tl -> (
                match hd with
                | Var a -> if a = x then true else iter tl
                | Power (a, b) ->
                    if a = x then if b = 0 then iter tl else true else iter tl
                | _ -> iter tl )
          in
          iter l
        in

        let rec iter (tmp : aexp list) : aexp list =
          match tmp with [] -> [] | hd :: tl -> d hd tmpBool :: iter tl
        in
        Times (iter l)
    | Power (a, b) ->
        if a = x then
          if b = 1 then Const 1
          else if b = 0 then if isTimesWithX then Const 1 else Const 0
          else Times [ Const b; Power (a, b - 1) ]
        else if isTimesWithX then Power (a, b)
        else Const 0
    | Var a ->
        if a = x then Const 1 else if isTimesWithX then Var a else Const 0
    | Const a -> if isTimesWithX then Const a else Const 0
  in
  d e false
