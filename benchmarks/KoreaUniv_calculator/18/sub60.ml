type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

type env = No_X | Xval of int;;
exception No_X_val;;

let calculator : exp -> int
= fun exp ->
  let rec _calc exp env = 
    match exp with
    | X -> (match env with
            | No_X -> raise No_X_val
            | Xval n -> n)
    | INT n -> n
    | ADD (exp1, exp2) -> (_calc exp1 env) + (_calc exp2 env)
    | SUB (exp1, exp2) -> (_calc exp1 env) - (_calc exp2 env)
    | MUL (exp1, exp2) -> (_calc exp1 env) * (_calc exp2 env)
    | DIV (exp1, exp2) -> (_calc exp1 env) / (_calc exp2 env)
    | SIGMA (exp1, exp2, exp3) ->
      let a = _calc exp1 env in
      let b = _calc exp2 env in
      let rec g i j sum =
        if i > j
        then sum 
        else g (i+1) j (sum + (_calc exp3 (Xval i)))  
      in g a b 0
  in _calc exp No_X;;
  
  