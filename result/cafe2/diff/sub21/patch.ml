exception InvalidArgument

type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec r_diff ((aexp : aexp), (str : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var v -> if v = str then Const 1 else Const 0
  | Power (v, i) ->
      if not (v = str) then Const 0
      else if i = 0 then Const 0
      else if i = 1 then Const 1
      else if i = 2 then Times [ Const i; Var v ]
      else Times [ Const i; Power (v, i - 1) ]
  | Times hd :: tl ->
      if tl = [] then r_diff (hd, str)
      else
        Sum
          [
            Times (r_diff (hd, str) :: tl); Times [ hd; r_diff (Times tl, str) ];
          ]
  | Times [] -> raise InvalidArgument
  | Sum hd :: tl ->
      if tl = [] then r_diff (hd, str)
      else Sum [ r_diff (hd, str); r_diff (Sum tl, str) ]
  | Sum [] -> raise InvalidArgument


let rec contain_zero (lst : aexp list) : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if hd = Const 0 then true else contain_zero tl


let rec minimize_sum (lst : aexp list) : aexp list =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Const 0 -> minimize_sum tl
      | Sum lst -> lst @ minimize_sum tl
      | _ -> hd :: minimize_sum tl )


let rec minimize_times (lst : aexp list) : aexp list =
  match lst with [] -> [] | hd :: tl -> hd :: minimize_times tl


let rec minimize (aexp : aexp) : aexp =
  match aexp with
  | Const i -> Const i
  | Var v -> Var v
  | Power (v, i) -> Power (v, i)
  | Sum lst ->
      if lst = [] then Const 0
      else if List.length lst = 1 then List.hd lst
      else Sum (List.map minimize (minimize_sum lst))
  | Times lst ->
      if lst = [] then Const 0
      else if contain_zero lst then Const 0
      else if List.length lst = 1 then List.hd lst
      else Times (List.map minimize (minimize_times lst))


let rec r_minimize (aexp : aexp) : aexp =
  if aexp = minimize aexp then aexp else r_minimize (minimize aexp)


let diff ((aexp : aexp), (str : string)) : aexp =
  r_minimize (r_diff (aexp, str))
