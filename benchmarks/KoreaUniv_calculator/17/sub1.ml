
(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec f e lst = match lst with
                         |[] -> begin match e with 
                                | X ->  0
                                | INT n -> n
                                | ADD(m, n) -> f m lst + f n lst
                                | SUB(m, n) -> f m lst - f n lst
                                | MUL(m, n) -> f m lst * f n lst
                                | DIV(m, n) -> if m = X || n = X then 0 else f m lst / f n lst 
                                | SIGMA(l, m, n) -> let a = f l lst in
                                                    let b = f m lst in
                                                    let rec makelst x y = if x>y then [] else
                                                                          (INT x)::(makelst (x+1) y) in
                                                    f n (makelst a b) end
                          |h::t -> begin match e with
                               | X -> f h []
                               | INT n -> n
                               | ADD(m, n) -> if t = [] then f m [h] + f n [h] else f m [h] + f n [h] + f e t
                               | SUB(m, n) -> if t = [] then f m [h] - f n [h] else f m [h] - f n [h] + f e t
                               | MUL(m, n) -> if t = [] then f m [h] * f n [h] else f m [h] * f n [h] + f e t
                               | DIV(m, n) -> if t = [] then f m [h] / f n [h] else f m [h] / f n [h] + f e t
                               | SIGMA(l, m, n) -> let a = f l [h] in
                                                   let b = f m [h] in
                                                   let rec makelst x y = if x>y then [] else
                                                                         (INT x)::(makelst (x+1) y) in
                                                   if t = [] then f n (makelst a b) else f n (makelst a b) + f e t end in   
                           f e []      
