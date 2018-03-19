type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec eval : exp -> int -> int
  = fun e i ->
    match e with
        X -> i
      | INT(a) -> a
      | ADD(a, b) -> eval a i + eval b i
      | SUB(a, b) -> eval a i - eval b i
      | MUL(a, b) -> eval a i * eval b i
      | DIV(a, b) -> eval a i / eval b i
      | SIGMA(a, b, c) -> 
          let aa = eval a 0 in
            if aa <= eval b 0
            then eval (SIGMA(INT(aa+1), b, c)) 0 + eval c aa
            else 0
      | _ -> raise(Failure "IDK")
;;


let rec calculator : exp -> int
  = fun e -> 
    match e with
        X -> raise(Failure "Variable Only")
      | _ -> eval e 0
;;



























