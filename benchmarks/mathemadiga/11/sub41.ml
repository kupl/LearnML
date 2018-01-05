(*��ǻ�Ͱ��к� 2009-11833 â�輺*)

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

(* X�� ��� ó���ؾ� �ϴ°�? X�� �����ϴ� �Լ��� ���� �޴� �Լ��� ���� ����� ��Ų��??*)

let rec mathemadiga x =
	match x with
	X -> X
	| INT a -> a
	| REAL b -> b
	| ADD (INT c, INT d) -> a + b
	| ADD (REAL c, REAL d) -> c +. d
	| SUB (INT a, INT b) -> a - b
	| SUB (REAL c, REAL d) -> c -. d
	| MUL (INT a, INT b) -> a * b
	| MUL (REAL c, REAL d) -> c *. d
	| DIV (INT a, INT b) -> a / b
	| DIV (REAL c, REAL d) -> c /. d
	| SIGMA (a, b, c) ->
		if mathemadiga a > mathemadiga b then raise ( "invalid input")
		else if mathemadiga a = mathemadiga b then let X = a in mathemadiga c
		else (let X = a in mathemadiga c) + mathemadiga (SIGMA (ADD(a , INT 1), b, c))
	| INTEGRAL (a, b, c) -> 
		if mathemadiga a < mathemadiga b then (0.1 *. (let X = a in mathemadiga c) +. mathemadiga (INTEGRAL ( ADD( a , REAL 0.1), b, c)))
		else if mathemadiga a = mathemadiga b then let X = a in mathemadiga c
		else raise ( "invalid input")
	| _ -> raise ("invalid input")