
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec lengthOf l = match l with
  | [] -> 0
  | hd :: tl -> 1 + lengthOf tl

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Const _ -> Const 0
  | Var x -> if (x = var) then Const 1 else Const 0
  | Power (x, p) ->
      if (x = var && p != 0) then Times [Const p; Power (x, p - 1)]
      else Const 0
  | Times e ->
      let rec timesDiff (Sum left, right, i) = match right with
      | [] -> Sum left
      | hd :: tl ->
        if i = 0 then Sum left
        else timesDiff (Sum (left @ [Times (diff (hd, var) :: tl)]), tl @ [hd], i - 1)
          in timesDiff (Sum [], e, lengthOf e)
  | Sum e ->
      let rec sumDiff (Sum left, right) = begin match right with
      | [] -> Sum left
      | hd :: tl -> sumDiff(Sum (left @ [diff (hd, var)]), tl)
   		end
    in sumDiff (Sum [], e)
