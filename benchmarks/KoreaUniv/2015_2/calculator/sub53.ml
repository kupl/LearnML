type exp = X
|INT of int
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp;;


let rec calculator : exp -> int
= fun f ->
match f with
|INT a -> a
|ADD (a,b) -> calculator a + calculator b
|SUB (a,b) -> calculator a - calculator b
|MUL (a,b) -> calculator a * calculator b
|DIV (a,b) -> calculator a / calculator b
|SIGMA(INT a,INT b,f) -> calculator a + SIGMA(a+1,b,f) ;;
