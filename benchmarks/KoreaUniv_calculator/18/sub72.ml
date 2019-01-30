type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
    |X -> 0
    |INT n -> n
    |ADD (a,b) -> calculator a + calculator b
    |SUB (a,b) -> calculator a - calculator b
    |MUL (a,b) -> calculator a * calculator b
    |DIV (a,b) -> calculator a / calculator b
    |SIGMA (a,b,c) -> 
      let rec calSigma exp num= 
        match exp with
          |X -> num
          |INT n -> n
          |ADD (a,b) -> calSigma a num + calSigma b num
          |SUB (a,b) -> calSigma a num - calSigma b num
          |MUL (a,b) -> calSigma a num * calSigma b num
          |DIV (a,b) -> calSigma a num / calSigma b num
          |_-> calculator exp
        in let rec range a b =
            if a = b then calSigma c a else
              calSigma c a + range (a+1) b
              in range (calculator a) (calculator b);;
   