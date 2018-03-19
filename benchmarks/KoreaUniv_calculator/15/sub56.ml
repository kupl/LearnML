type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
= fun e ->
let rec fnc : exp -> (int -> int)
= fun s ->
match s with 
|X -> (fun x -> x)
|INT(i) -> (fun x -> i)
|ADD(a,b) -> (fun x -> ((fnc a) x) + ((fnc b) x))
|MUL(a,b) -> (fun x -> ((fnc a) x) * ((fnc b) x))
|SUB(a,b) -> (fun x -> ((fnc a) x) - ((fnc b) x))
|DIV(a,b) -> (fun x -> ((fnc a) x) / ((fnc b) x))
in let rec result a b f =
if a=b then f a
else (f a)+(result (a+1) b f)
in match e with
|SIGMA(INT(a),INT(b),c) -> result a b (fnc c) 
