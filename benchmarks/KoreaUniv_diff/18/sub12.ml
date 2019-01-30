type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  let rec diff_sum lst = 
    match lst with
      | [] -> []
      | hd::tl -> (diff (hd,x))::(diff_sum tl) in
  
  match exp with
    | Const c -> Const 0
    | Var s -> if s=x then Const 1 else Const 0
    | Power (s, i) -> if s=x then Times [Const i; Power (s, i-1)] else Power (s, i)
    | Times lst -> begin match lst with
      | [] -> Const 0
      | hd::tl -> Sum ((Times ((diff (hd, x))::tl))::[Times (hd::[diff ((Times tl), x)])])
      end
    | Sum lst -> begin match lst with
      | [] -> Const 0
      | hd::tl -> Sum (diff_sum lst)
      end;;
  
  
diff (Sum [Power ("x",2); Times [Const 2; Var "x"]; Const 1], "x");;

