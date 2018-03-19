type exp = X 
         | INT of int 
         | ADD of exp * exp 
         | SUB of exp * exp 
         | MUL of exp * exp 
         | DIV of exp * exp 
         | SIGMA of exp * exp * exp 
let calculator : exp -> int 
=fun e ->  (* TODO *) 
    let exptoint exp = 
        match exp with 
        INT n -> n in 
            match e with 
            ADD(e1,e2) -> exptoint e1 + exptoint e2 
            |SUB(e1,e2) -> exptoint e1 - exptoint e2 
            |MUL(e1,e2) -> exptoint e1 * exptoint e2 
            |DIV(e1,e2) -> exptoint e1 / exptoint e2;; 