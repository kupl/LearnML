(* 2 *)
exception InvalidArgument
type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

let rec diff (exp, str) = 
        match exp with
        | Const c -> (Const 0)
        | Var x ->
          if x = str then (Const 1)
          else (Const 0)
        | Power (x, p) ->
          if p = 0 then (Const 0)
          else if x = str then (Times [(Const p); Power(x, p-1)])
          else (Const 0)
        | Times l ->
          (match l with
          | [] -> raise InvalidArgument
          | _ -> (Sum (make_times_list [] l str))
          )
        | Sum l -> 
          (match l with
          | [] -> raise InvalidArgument
          | _ -> (Sum (make_sum_list l str))
          )

and make_times_list prefix l x = 
        match l with
        | [] -> []
        | h::t ->
          let d = diff(h, x) in
          if d = (Const 0) then (make_times_list (h::prefix) t x)
          else Times(d::(List.append prefix t))::(make_times_list (h::prefix) t x)

and make_sum_list l x = 
        match l with
        | [] -> []
        | h::t -> diff(h, x)::(make_sum_list t x)