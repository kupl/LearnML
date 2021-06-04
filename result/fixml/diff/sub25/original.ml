type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff (aexp, instr) =
  match aexp with
  | Const a -> Const 0
  | Var x -> if x = instr then Const 1 else Const 0
  | Power (x, y) ->
      if x = instr && y = 0 then Const 0
      else if x = instr && y != 0 then Times [ Const y; Power (x, y - 1) ]
      else Const 0
  | Times [] -> raise InvalidArgument
  | Times lst -> timeProcess (lst, instr)
  | Sum [] -> raise InvalidArgument
  | Sum lst -> Sum (List.map (fun x -> diff (x, instr)) lst)


and timeProcess (l, istr) =
  let check x =
    match x with
    | Var x -> if x = istr then true else false
    | Power (x, _) -> if x = istr then true else false
    | _ -> false
  in

  let varList = List.filter (fun x -> check x) l in
  if List.exists (fun x -> check x) l then
    Times
      ( diff (polyProcess (varList, istr), istr)
      :: List.filter (fun x -> not (check x)) l )
  else Const 0


and polyProcess (l, is) =
  let varToPower __fun__ =
    match __fun__ with
    | Var x -> Power (x, 1)
    | Power (x, y) -> Power (x, y)
    | _ -> raise InvalidArgument
  in

  let powerFold x __fun__ =
    match __fun__ with Power (_, a) -> x + a | _ -> raise InvalidArgument
  in

  let poweredList = List.map (fun x -> varToPower x) l in

  let rec fold_left f a l =
    match l with [] -> a | h :: t -> fold_left f (f a h) t
  in

  let muled = fold_left powerFold 0 poweredList in
  Power (is, muled)
