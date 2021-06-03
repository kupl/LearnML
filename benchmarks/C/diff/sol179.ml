(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) ->
  match e with
  | Const i -> Const 0
  | Var y  -> if y = x then (Const 1) else (Const 0)
  | Power (y, i) -> if y = x then Times [(Const i); (Power (y, (i-1)))]
                    else (Const 0)
  | Times l -> (match l with
                | [] -> (Const 0)
                | [single_e] -> (diff (single_e, x))
                | hd::tl -> Sum [Times ((diff (hd, x))::tl); Times ((hd)::[(diff ((Times tl), x))])]
               )
  | Sum l -> let derived_list = List.fold_left (fun acc e_prime -> acc @ [diff (e_prime, x)]) [] l in
             Sum derived_list