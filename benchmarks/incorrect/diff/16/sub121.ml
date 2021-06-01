
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
    | Const i -> Const 0
    | Var s -> if (s = var) then Const 1 else Const 0
    | Power (s, i) -> if (s = var) then Times [Const i ; Power (s, i-1)] else Const 0
    | Times lst -> begin match lst with 
      | [] -> Const 1 
      | hd::tl -> Times [diff_times_helper (hd, var) ; diff (Times tl, var)]
    end
    | Sum lst -> begin match lst with 
      | [] -> Const 0
      | hd::tl -> Sum [diff (hd, var) ; diff (Sum tl, var)]
    end

  and diff_times_helper (exp, var) =
    match exp with 
      | Const i -> Const i
      | Var s -> if (s = var) then Const 1 else Var s
      | Power (s, i) -> if (s = var) then Times [Const i ; Power (s, i-1)] else Power(s, i)
      | _ -> diff (exp, var)
