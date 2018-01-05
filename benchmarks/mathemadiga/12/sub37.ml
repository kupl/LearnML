exception FreeVariable
exception InvalidSigma
exception Error of string
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

module Value =
struct
	type element = Int of int | Real of float
	let is_int v = 
		match v with
		Int i -> true
		|_ -> false

	let is_float v =
		match v with
		Real f -> true
		|_ -> false

	let get_int v =
		match v with
		Int i -> i
		| Real f -> int_of_float f

	let get_float v =
		match v with
		Int i -> float_of_int i
		| Real f -> f

	let add v1 v2 =
		if is_int v1 && is_int v2 then
			Int(get_int v1 + get_int v2)
		else
			Real(get_float v1 +. get_float v2)

	let sub v1 v2 =
		if is_int v1 && is_int v2 then
			Int(get_int v1 - get_int v2)
		else
			Real(get_float v1 -. get_float v2)

	let mul v1 v2 =
		if is_int v1 && is_int v2 then
			Int(get_int v1 * get_int v2)
		else
			Real(get_float v1 *. get_float v2)

	let div v1 v2 =
		Real(get_float v1 /. get_float v2)

	let equal v1 v2 =
		if is_int v1 && is_int v2 then
			get_int v1 = get_int v2
		else
			abs_float(get_float v1 -. get_float v2) < 0.0001
	let gt v1 v2 =
		if is_int v1 && is_int v2 then
			get_int v1 > get_int v2
		else
			not(equal v1 v2) && (get_float v1 > get_float v2)

	let lt v1 v2 =
		if is_int v2 && is_int v2 then
			get_int v1 < get_int v2
		else
			not(equal v1 v2) && (get_float v1 < get_float v2)

	let gte v1 v2 =
		equal v1 v2 || gt v1 v2
	
	let lte v1 v2 =
		equal v1 v2 || lt v1 v2

	let not_equal v1 v2 =
		not(equal v1 v2)

	let i v =
		Int(v)

	let f v =
		Real(v)

	let is_i v =
		equal (i (get_int v)) v
end

let rec mathemadiga e =
	let rec sigma b u e =
		let eval e x =
			try
				calc e
			with
				FreeVariable -> (Value.i x)
		in
		if b > u then
			Value.i 0
		else
			Value.add (eval e b) (sigma (b + 1) u e)
	and
	integral b u e =
		let eval e x =
			let r =
				try
					calc e
				with
					FreeVariable -> (Value.f x)
			in
			Value.f (Value.get_float r *. 0.1)
		in
		if b > u then
			Value.i 0
		else
			Value.add (Value.mul (Value.f 0.1) (eval e b)) (integral (b +. 0.1) u e)
	and
	calc e =
		match e with
		X -> raise FreeVariable
		| INT i -> Value.i i
		| REAL f -> Value.f f
		| ADD (e1, e2) -> Value.add (calc e1) (calc e2)
		| SUB (e1, e2) -> Value.sub (calc e1) (calc e2)
		| MUL (e1, e2) -> Value.mul (calc e1) (calc e2)
		| DIV (e1, e2) -> Value.div (calc e1) (calc e2)
		| SIGMA (e1, e2, e3) ->
			let a, b = (calc e1), (calc e2) in
			if Value.gt a b then
				Value.i 0
			else
				sigma (Value.get_int a) (Value.get_int b) e3
		| INTEGRAL (e1, e2, e3) ->
			let a, b = (calc e1), (calc e2) in
			integral (Value.get_float a) (Value.get_float b) e3
	in
	Value.get_float (calc e)
