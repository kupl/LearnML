
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> match exp with 
    | X -> raise (Failure "Value for X has not been provided") (* ?? *)
    | INT i -> i
    | ADD (e1, e2) -> calculator e1 + calculator e2
    | SUB (e1, e2) -> calculator e1 - calculator e2
    | MUL (e1, e2) -> calculator e1 * calculator e2
    | DIV (e1, e2) -> calculator e1 / calculator e2
    | SIGMA (e1, e2, e3) -> let result = ref 0 in (* gotta make a ref cos ocaml doesnt update regular values *)
      for i = calculator e1 to calculator e2 do
        result := !result + sigma_helper e3 i
      done;
      !result
  and sigma_helper exp i = match exp with 
    | X -> i 
    | INT i1 -> i1
    | ADD (e1, e2) -> sigma_helper e1 i + sigma_helper e2 i
    | SUB (e1, e2) -> sigma_helper e1 i - sigma_helper e2 i
    | MUL (e1, e2) -> sigma_helper e1 i * sigma_helper e2 i
    | DIV (e1, e2) -> sigma_helper e1 i / sigma_helper e2 i
    | SIGMA (e1, e2, e3) -> let result = ref 0 in
      for i = sigma_helper e1 i to sigma_helper e2 i do
        result := !result + sigma_helper e3 i
      done;
      !result;;