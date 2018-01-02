{
	MUL (MUL (INT (3), INT (10)), ADD (SUB (INT (3), INT (1)), DIV (INT (10), INT (2)))) -> 210;
	MUL (SIGMA (INT (1), INT (10), SUB (MUL (X, X), INT (1))), SIGMA (INT (1), INT (10), SUB (MUL(X, X), INT (1)))) -> 140625;
	SIGMA (DIV (INT (10), INT (2)), MUL (ADD (INT (3), INT (10)),INT (1)), SUB (X, INT (1))) -> 72;
	SIGMA (SIGMA (INT (1),INT (5),X), SIGMA (INT (3), INT (10), X), MUL (X, MUL (X,X))) -> 1887859;
	SIGMA (SIGMA (INT (1),SIGMA (INT (1), INT (2), X), X), SIGMA (INT (3), INT (10), X), SIGMA (INT (1), INT (10), X)) -> 2585;
}
type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec convertVar : exp -> int -> exp
= fun exp i ->
	match exp with
	| X -> INT (i)
	| INT (n) -> INT (n)
	| ADD (exp1, exp2) -> ADD ((convertVar exp1 i), (convertVar exp2 i))
	| SUB (exp1, exp2) -> SUB ((convertVar exp1 i), (convertVar exp2 i))
	| MUL (exp1, exp2) -> MUL ((convertVar exp1 i), (convertVar exp2 i))
	| DIV (exp1, exp2) -> DIV ((convertVar exp1 i), (convertVar exp2 i))
	| SIGMA (exp1, exp2, exp3) -> SIGMA ((convertVar exp1 i), (convertVar exp2 i), exp3)
;;

let rec f : exp -> int
= fun exp -> 
	match exp with
	| INT (i) -> i
	| ADD (exp1, exp2) -> (f exp1) + (f exp2)
	| SUB (exp1, exp2) -> (f exp1) - (f exp2)
	| MUL (exp1, exp2) -> (f exp1) * (f exp2)
	| DIV (exp1, exp2) -> (f exp1) / (f exp2)
	| SIGMA (i, j, exp) ->
		let start = f i in
			if (f j) = start
				then f (convertVar exp start)
			else 
				(f (convertVar exp start)) + (f (SIGMA(ADD (i, INT (1)), j, exp)))
;;