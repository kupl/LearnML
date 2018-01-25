type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec repl_X_int : exp * exp -> exp
=fun (e,n) -> match e with
  | X -> n
  | INT x -> INT x
  | ADD (e1,e2) -> ADD (repl_X_int (e1,n),repl_X_int (e2,n))
  | SUB (e1,e2) -> SUB (repl_X_int (e1,n),repl_X_int (e2,n))
  | MUL (e1,e2) -> MUL (repl_X_int (e1,n),repl_X_int (e2,n))
  | DIV (e1,e2) -> DIV (repl_X_int (e1,n),repl_X_int (e2,n))
  | SIGMA (e1,e2,e3) -> SIGMA (repl_X_int (e1,n),repl_X_int (e2,n), repl_X_int(e3,n))

let rec calculator : exp -> int
=fun e -> match e with
  | X -> raise (Failure "error")
  | INT n -> n
  | ADD (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c + d
      | (INT c, e2) -> c + calculator e2
      | (e1, INT d) -> calculator e1 + d
      | (e1, e2) -> calculator e1 + calculator e2)
  | SUB (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c - d
      | (INT c, e2) -> c - calculator e2
      | (e1, INT d) -> calculator e1 - d
      | (e1, e2) -> calculator e1 - calculator e2)
  | MUL (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c * d
      | (INT c, e2) -> c * calculator e2
      | (e1, INT d) -> calculator e1 * d
      | (e1, e2) -> calculator e1 * calculator e2)
  | DIV (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c / d
      | (INT c, e2) -> c / calculator e2
      | (e1, INT d) -> calculator e1 / d
      | (e1, e2) -> calculator e1 / calculator e2)
  | SIGMA (a, b, e3) -> (match e3 with
      | SIGMA (_,_,_) -> calculator (SIGMA (a, b, INT (calculator e3)))
      | _ -> (match (a,b) with
          | (INT c, INT d) ->
                if c = d then calculator (repl_X_int (e3,INT d))
                else calculator (repl_X_int (e3,INT c)) + calculator (SIGMA ((INT (c+1), INT d, e3)))
          | (INT c, e2) -> calculator (SIGMA (INT c, INT (calculator e2), e3))
          | (e1, INT d) -> calculator (SIGMA (INT (calculator e1), INT d, e3))
          | (e1, e2) -> calculator (SIGMA (INT (calculator e1), INT (calculator e2), e3))))
