(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list


let rec diff : aexp * string -> aexp

= fun (e,x) ->(*x is parameter*) 
match e with
|Const c -> Const 0
|Var f -> if f = x then (Const 1) else (Const 0)
|Power (a,b) -> if a = x then Times[Const b; Power(a,(b-1))] else (Const 0)
|Times l -> (match l with
            |[]-> raise (NOANSWER)
            |[a]-> Times [diff(a,x)]
            |h::t -> Sum [Times (diff(h,x)::t);Times (h::[diff(Times t,x)])])
                                            
|Sum l -> (match l with
              |[]-> Const 0
              |h::t -> Sum(diff(Sum t, x)::[diff(h,x)]))
