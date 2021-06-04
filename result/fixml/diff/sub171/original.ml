type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  let cleanTimes : aexp -> aexp =
   fun exps ->
    match exps with
    | Times xs ->
        if List.length xs = List.length (List.filter (fun x -> x != Const 0) xs)
        then
          if List.length (List.filter (fun x -> x != Const 1) xs) = 1 then
            List.hd (List.filter (fun x -> x != Const 1) xs)
          else Times (List.filter (fun x -> x != Const 1) xs)
        else Const 0
    | _ -> exps
  in

  let cleanPower : aexp -> aexp =
   fun exps ->
    match exps with Power (a, n) -> if n = 1 then Var a else exps | _ -> exps
  in

  let cleanSum : aexp -> aexp =
   fun exps ->
    match exps with
    | Sum xs -> if List.length xs = 1 then List.hd xs else exps
    | _ -> exps
  in

  match exp with
  | Const n -> Const 0
  | Sum xs ->
      cleanSum
        (Sum
           (List.filter
              (fun x -> x != Const 0)
              (List.map (fun a -> diff (a, x)) xs)))
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, n) ->
      if a = x then
        cleanTimes (Times [ Const n; cleanPower (Power (a, n - 1)) ])
      else Const 0
  | Times xs ->
      cleanSum
        (Sum
           (List.filter
              (fun x -> x != Const 0)
              (List.map
                 (fun a ->
                   cleanTimes
                     (Times
                        (List.map
                           (fun b -> if b = a then diff (b, x) else b)
                           xs)))
                 xs)))
