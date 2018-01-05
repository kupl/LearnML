type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

exception Error



let rec insert(f, value) =
	match f with
	| X -> value
	| INT a -> float_of_int(a)
	| REAL a -> a
	| ADD(e1, e2) -> insert(e1, value) +. insert(e2, value)
	| SUB(e1, e2) -> insert(e1, value) -. insert(e2, value)
	| MUL(e1, e2) -> insert(e1, value) *. insert(e2, value)
	| DIV(e1, e2) -> 
					if(insert(e2, value) = 0.) then raise Error
					else insert(e1, value) /. insert(e2, value)
	| SIGMA(e1, e2, e3) -> 
						begin
						match (e1, e2) with
						| (REAL a, _) | (_, REAL a) -> raise Error
						| (X, _) | (_, X) -> raise Error
						| _ -> sigma(insert(e1, value), insert(e2, value), e3)
						end

	| INTEGRAL(e1, e2, e3) -> integral(insert(e1, value), insert(e2, value), e3)

	and sigma(base, top, f)=
		if base>top|| base<0. || top<0. then raise Error
		else if base=top then insert(f, top)
		else (insert(f, base))+.sigma((base+.1.), top, f)

	and integral(base, top, f)=
		if base=top then 0.
		else if base>top then 0.-.(integral(top, base, f))
		else if top-.base < 0.1 then insert(f, base)*.(top-.base)
		else (insert(f,base)*.(0.1)) +. (integral(base+.(0.1), top, f))


let rec sigma(base, top, f)=
	if base>top|| base<0. || top<0. then raise Error
	else if base=top then insert(f, top)
	else (insert(f, base))+.sigma((base+.1.), top, f)

let rec integral(base, top, f)=
	if base=top then 0.
	else if base>top then 0.-.(integral(top, base, f))
	else if top-.base < 0.1 then insert(f, base)*.(top-.base)
	else (insert(f,base)*.(0.1)) +. (integral(base+.(0.1), top, f))




let rec mathemadiga exp =
	match exp with
	| X -> raise Error
	| INT a -> float_of_int(a)
	| REAL a -> a
	| ADD(e1, e2) -> mathemadiga(e1) +. mathemadiga(e2)
	| SUB(e1, e2) -> mathemadiga(e1) -. mathemadiga(e2)
	| MUL(e1, e2) -> mathemadiga(e1) *. mathemadiga(e2)
	| DIV(e1, e2) -> 
					if(mathemadiga(e2) = 0.) then raise Error
					else mathemadiga(e1) /. mathemadiga(e2)

	| SIGMA(e1, e2, e3) -> 
						begin
						match (e1, e2) with
						| (REAL a, _) | (_, REAL a) -> raise Error
						| (X, _) | (_, X) -> raise Error
						| _ -> sigma(mathemadiga(e1), mathemadiga(e2), e3)
						end

	| INTEGRAL(e1, e2, e3) ->
							begin
							match (e1, e2) with
							| (X, _) | (_, X) -> raise Error
							| _ -> integral(mathemadiga(e1), mathemadiga(e2), e3)
							end
	
