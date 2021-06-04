type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff ((aexp : aexp), (instr : string)) : aexp =
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
  | Sum lst -> Sum (List.map (fun (x : aexp) -> diff (x, instr)) lst)


and timeProcess ((l : aexp list), (istr : string)) : aexp =
  let check (x : aexp) : bool =
    match x with
    | Var x -> if x = istr then true else false
    | Power (x, _) -> if x = istr then true else false
    | _ -> false
  in

  let varList : aexp list = List.filter (fun (x : aexp) -> check x) l in

  match l with
  | [] -> Const 0
  | __s10 :: __s11 ->
      Sum
        [
          Times ([ diff (__s10, istr) ] @ __s11);
          Times [ __s10; timeProcess (__s11, istr) ];
        ]


and polyProcess ((l : aexp list), (is : string)) : aexp =
  let varToPower (__fun__ : aexp) : aexp =
    match __fun__ with
    | Var x -> Power (x, 1)
    | Power (x, y) -> Power (x, y)
    | _ -> raise InvalidArgument
  in

  let powerFold (x : int) (__fun__ : aexp) : int =
    match __fun__ with Power (_, a) -> x + a | _ -> raise InvalidArgument
  in

  let poweredList : aexp list = List.map (fun (x : aexp) -> varToPower x) l in

  let rec fold_left (f : int -> aexp -> int) (a : int) (l : aexp list) : int =
    match l with [] -> a | h :: t -> fold_left f (f a h) t
  in

  let muled : int = fold_left powerFold 0 poweredList in
  Power (is, muled)
