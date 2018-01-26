(*problem 5*)
type exp = X
				| INT of int
				| ADD of exp * exp
			  | SUB of exp * exp
			  | MUL of exp * exp
			  | DIV of exp * exp
        | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e ->
    let rec sigma (n, m, f)    
		 = if n = m  then f n else (f n) + (sigma (n+1, m, f))
	      in let rec change_x : exp -> (int -> int)
           = fun changex -> match changex with
             |X -> (fun x -> x)
             |INT n -> (fun x -> n)
             |ADD (n, m) -> (fun x -> ((change_x n)x)+((change_x m)x))
             |SUB (n, m) -> (fun x -> ((change_x n)x)-((change_x m)x))
             |MUL (n, m) -> (fun x -> ((change_x n)x)*((change_x m)x))
             |DIV (n, m) -> (fun x -> ((change_x n)x)/((change_x m)x))
             |SIGMA (n, m, f) -> (fun x -> sigma((change_x n)x, (change_x m)x, (change_x f)))
in match e with
X -> raise (Failure "X is not int")
|INT n -> n
|ADD (n, m) -> (calculator n) + (calculator m)
|SUB (n, m) -> (calculator n) - (calculator m)
|MUL (n, m) -> (calculator n) * (calculator m)
|DIV (n, m) -> (calculator n) / (calculator m)
|SIGMA (n, m, exp) -> sigma (calculator n, calculator m, change_x exp)