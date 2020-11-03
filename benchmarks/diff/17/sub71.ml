(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
;;
let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
| Const n -> Const 0
| Var s -> if s=x then Const 1
           else Const 0
| Power (s,n) -> if s=x then 
                    if n=0 then Const 0
                    else Times [Const n; Power (s,n-1)]
                 else Const 0
| Times l -> begin
              match l with
              | [] -> Const 0
              | hd::tl -> Sum ((Times((diff (hd,x))::tl))::([Times((diff (Times tl,x))::[hd])]))
             end
| Sum k ->
          match k with
          | []-> Const 0
          | hd::tl -> Sum ((diff (hd,x))::([diff (Sum tl,x)]));;
