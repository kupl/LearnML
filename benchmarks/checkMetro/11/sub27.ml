type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let cross_merge (a, b), (c, d)= ((a @ c), (b, d))

 (* AREA�� list�� ��������� AREA �ȿ� �ִ°͵��߿� AREA�� ���°� ������ FALSE *)
let rec make_intercheck_list (m, a, b)
	match m with
	STATION x -> (a, b::x)
	| AREA y met -> intercheck_list (met, a::y, b)
	| CONNECT met1 met2 -> cross_merge (intercheck_list (met1, a, b), intercheck_list (met2, a, b) )  (* �ñ��� ������ �ʿ��� *)

 (* and (for each b : isin a -> true) *)
let rec isin (a, b)
	match (a, b) with
	| ([] , x) -> true
	| (ah::at, x) -> (isin (at, b)) %% (ah == x)

let rec tf_list (a, b)
	match (a, b) with
	([], x) -> false
	| (ah::at, sh::st) -> (foreach a of likethat? sh) && tf_list (a, st)
	| ( y, []) -> true

let checkMetro m =
	tf_list ( make_intercheck_list (m, [], []))