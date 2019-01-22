type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
  match aexp, x with
    Const c, _ -> Const 0
  | Var a, t -> if a = t then Const 1    (* f(x)d/dx = f'(x) *)
                else Const 0             (* f(x)d/dt = 0 *)
  | Power (a, b), t -> if a = t then Times [Const b; Power (a, b-1)]
                       else Const 0
  | Times lst, t -> (match lst with
                      [] -> Const 0
                    | head::[] -> diff (head, t)
                    | head::tail -> Sum [(Times ([diff (head, t)]@ tail)); (Times [head; diff (Times tail, t)])])
  | Sum lst, t -> (match lst with
                    [] -> Const 0
                  | head::[] -> diff (head, t)
                  | head::tail -> Sum ([diff (head, t)]@ [diff (Sum tail, t)]))
                  

