
type exp = X
	|INT of int
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp


let rec calculator : exp-> int =
fun f ->
match f with
|X-> 0
|INT a -> a
|ADD (e1, e2) -> calculator (e1) + calculator (e2)
|SUB (e1, e2) -> calculator (e1) - calculator (e2)
|MUL (e1, e2) -> calculator (e1) * calculator (e2)
|DIV (e1, e2) -> calculator (e1) / calculator (e2)
|SIGMA (e1, e2, e3) ->
match e3 with
  |INT a -> calculator(MUL(ADD(SUB(e2,e1),INT 1),e3))
  |X -> calculator(SUB(DIV(MUL(e2,ADD(e2,INT 1)),INT 2), DIV(MUL(e1,SUB(e1,INT 1)),INT 2)))