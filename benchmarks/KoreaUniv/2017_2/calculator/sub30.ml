
(* problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->  
let rec calc : exp -> int -> int
= fun ex i -> (match ex with
  | X -> i
  | INT k -> k
  | ADD (a,b) -> (calc a i) + (calc b i)
  | SUB (a,b) -> (calc a i) - (calc b i)
  | MUL (a,b) -> (calc a i) * (calc b i)
  | DIV (a,b) -> if (calc b i) = 0 then raise (Failure "divide by 0 error")
                 else (calc a i) / (calc b i)
  | SIGMA (s, e, f) -> let sta = calc s i in 
                          let ed = calc e i in
                          if (sta = ed) then calc f sta
                          else (calc f sta) + (calc (SIGMA ((INT (sta+1)),INT ed,f))i))
in match e with
  | X -> raise(Failure "Error")
  | INT i -> i
  | ADD (a,b) -> calculator(a) + calculator(b)
  | SUB (a,b) -> calculator(a) - calculator(b)
  | MUL (a,b) -> calculator(a) * calculator(b)
  | DIV (a,b) -> if calculator(b) = 0 then raise (Failure "not divisiable by 0")
                 else calculator(a) / calculator(b)
  | SIGMA (sta, en, f) -> let st = calculator(sta) in
                              let ed = calculator(en) in
                                if (st = ed) then calc f st
																else (calc f st) + (calculator( SIGMA( INT (st + 1), INT ed, f)));;
