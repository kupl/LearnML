type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec sigma
= fun (f, a, b) ->
	if a=b then (f a)
	else (f b) + sigma (f, a, (b-1))

let rec  tofun : exp->(int->int)
	= fun ex -> match ex with
	|X ->(fun x->x)
	|INT n ->(fun x ->n )
	|ADD(e1, e2) ->(fun x->(((tofun e1) x) +((tofun e2) x )))
	|SUB(e1, e2) ->(fun x->(((tofun e1) x )-((tofun e2) x )))
	|MUL(e1, e2) ->(fun x->(((tofun e1) x )*((tofun e2) x )))
	|DIV(e1, e2) ->(fun x->(((tofun e1) x )/((tofun e2) x )))
	|_->(fun x->0)

let rec forcal : exp -> int
	= fun ex -> match ex with
	|INT n -> n
	|ADD(INT n1, INT n2) -> n1+n2
	|ADD(e1, e2) -> (forcal e1 + forcal e2)
	|SUB(INT n1, INT n2) -> n1-n2
	|SUB(e1, e2) -> forcal(e1)-forcal(e2)
	|MUL(INT n1, INT n2) -> n1*n2
	|MUL(e1, e2) -> forcal(e1)*forcal(e2)
	|DIV(INT n1, INT n2) -> n1/n2
	|DIV(e1, e2) -> forcal(e1)/forcal(e2)
	|SIGMA(INT n1, INT n2, e) -> sigma(tofun(e), n1, n2)
	|SIGMA(e1, e2, e3) ->sigma(tofun(e3), forcal(e1), forcal(e2))

let rec calculator : exp->int
	= fun exp -> match exp with
	|X->0
	|INT n ->n
	|ADD(INT n1, INT n2) ->n1+n2
	|ADD(e1, e2)->calculator(e1)+calculator(e2)
	|SUB(INT n1, INT n2) -> n1-n2
	|SUB(e1, e2) -> calculator(e1)-calculator(e2)
	|MUL(INT n1, INT n2) -> n1*n2
	|MUL(e1, e2)-> calculator(e1)*calculator(e2)
	|DIV(INT n1, INT n2) -> n1/n2
	|DIV(e1, e2)->calculator(e1)/calculator(e2)
	|SIGMA(INT n1, INT n2, e ) -> sigma(tofun(e), n1, n2)
	|SIGMA(e1, e2, e3) ->sigma(tofun(e3), calculator(e1), calculator(e2))
