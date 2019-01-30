type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;
  

let rec calculator: exp -> int
= fun exp -> let rec sigmac (n,m,f) = 
    if n=m then f n
    else (f n) + (sigmac (n+1,m,f))
    in let rec replace_x : exp -> (int->int) = 
      fun replacex -> match replacex with 
        |X -> (fun x -> x)
        |INT n -> (fun x -> n)
        |ADD (n,m) -> (fun x -> ((replace_x n)x)+((replace_x m)x))
        |SUB (n,m) -> (fun x -> ((replace_x n)x)-((replace_x m)x))
        |MUL (n,m) -> (fun x -> ((replace_x n)x)*((replace_x m)x))
        |DIV (n,m) -> (fun x -> ((replace_x n)x)/((replace_x m)x))
        |SIGMA (n,m,f) -> (fun x -> sigmac((replace_x n)x,(replace_x m)x,(replace_x f)))
        in match exp with X -> raise (Failure "Ang")
        | INT a -> a
        | ADD (a,b) -> calculator a + calculator b
        | SUB (a,b) -> calculator a - calculator b
        | MUL (a,b) -> calculator a * calculator b
        | DIV (a,b) -> calculator a / calculator b
        | SIGMA (n,m,ex) -> sigmac(calculator n,calculator m,replace_x ex);;
             

