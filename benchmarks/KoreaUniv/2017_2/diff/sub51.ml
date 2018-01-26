type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
  match e with 
  | Const(t) -> Const (0)
  | Var(t) -> if t = x then Const (1) else Var(t)
  | Power(t, p) -> if t = x then Times([Const(p)]@[Power(t, p-1)]) else Power(t,p)
  | Times(l) -> 
    (match l with
     | [] -> Const (1)
     | h :: t -> 
       (match h with 
       | Const(c) ->Times([Const(c)]@[diff(Times(t),x)])
       | _ -> Times([diff(h,x)]@[diff(Times(t),x)])))
  | Sum(l) ->
    (match l with
     | [] -> Const (0);
     | h :: t -> Sum([diff(h,x)]@[diff(Sum(t),x)]));;