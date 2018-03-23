type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

(*let rec diff_cln aexp = *)
let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
Const i -> Const 0
| Var v -> if v=x then Const 1 else Var v
| Power (v,p) ->if v=x then match p with
                 1 ->  Const 1
                |2 ->  Times [Const p; Var v]
                |_ ->  Times [Const p; Power (v, p-1)]
              else Power (v,p)
| Times l1 -> let rec iter l= match l with
              [] -> [] 
              | h::t -> let rec isConst l = match l with 
                    [] -> true (*if this continues, it reaches here*)
                    | h::t -> match h with 
                          Const c -> isConst t  
                    | _ -> false
                  in if isConst l then [Const 0] else 
              match h with 
                        Const c -> h::iter(t)
                       | Sum lx -> h::iter(t)
                       | _ -> diff(h,x)::iter(t)
              in Times (iter l1)  
| Sum l2 -> let rec iter l = match l with
              [] -> [] 
              | h::t -> match h with 
                    Const 0 -> iter(t)
                  |  Times [Const 0] -> iter(t)
                  | _ -> diff(h,x)::iter(t)
              in Sum (iter l2)