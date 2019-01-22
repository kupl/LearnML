type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  match aexp with
  | Const a -> Const 0
  | Var alpha -> if alpha = x then Const 1 else aexp
  | Power (alpha, a) -> if alpha = x then Times [Const a; Power (alpha, a-1)] else aexp
  | Sum lst -> (match lst with
    | [] -> Const 0
    | hd::tl -> (match tl with
      | [] -> diff(hd,x)
      | _ -> Sum (diff(hd,x)::diff(Sum tl,x)::[]) ))
  | Times lst -> (match lst with
    | [] -> Const 1
    | hd::tl -> (match tl with
      | [] -> diff(hd,x)
      | _ -> Sum (
        (Times (diff(hd,x)::(Times tl)::[]))
        ::(Times (hd::diff(Times tl,x)::[]))
        ::[])
      )
    )
