type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

exception FreeVariable
exception InvalidSigma
let rec mathemadiga exp = 
	let mkfloat num = 
		match num with
		|INT a -> float_of_int a
		|REAL a -> a
		|_ -> raise FreeVariable
	in
	let addis (e1,e2) = 
		if ((e1=X)||(e2=X)) then raise FreeVariable
		else mathemadiga(e1) +. mathemadiga(e2)
	in
	let subis (e1,e2) =
		if ((e1=X)||(e2=X)) then raise FreeVariable
		else mathemadiga(e1) -. mathemadiga(e2)
	in
	let mulis (e1,e2) =
		if ((e1=X)||(e2=X)) then raise FreeVariable		
		else mathemadiga(e1) *. mathemadiga(e2)
	in
	let divis (e1,e2) =
		if ((e1=X)||(e2=X)) then raise FreeVariable
		else mathemadiga(e1) /. mathemadiga(e2)
	in
	let rec sigmais (e1,e2,e3) =
		let mkint num = 
			match num with
			|INT a -> a
			|REAL a -> int_of_float a
			|_ -> raise InvalidSigma
		in
		let rec calc (e1,e2,e3) =
			match e3 with
			|X -> float(mkint e1)
			|INT a -> float(a)
			|REAL a -> a
			|ADD(a,b) -> calc(e1,e2,a) +. calc(e1,e2,b)
			|SUB(a,b) -> calc(e1,e2,a) -. calc(e1,e2,b)
			|MUL(a,b) -> calc(e1,e2,a) *. calc(e1,e2,b)
			|DIV(a,b) -> calc(e1,e2,a) /. calc(e1,e2,b)
			|SIGMA(a,b,c) -> sigmais((REAL (calc (e1,e2,a))),(REAL (calc (e1,e2,b))),c)
			|INTEGRAL(a,b,c) -> inteis((REAL (calc (e1,e2,a))),(REAL (calc (e1,e2,b))),c)
		in
		if ((mkint e1) > (mkint e2)) then 0.0
		else
		match e3 with
		|X -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)
		|ADD (a,b) -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)
		|MUL (a,b) -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)
		|SUB (a,b) -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)
		|DIV (a,b) -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)
		|SIGMA (a,b,c) -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)
		|INTEGRAL (a,b,c) -> calc(e1,e2,e3) +. sigmais(INT((mkint e1)+1),e2,e3)

	and inteis (e1,e2,e3) =
		let rec calc (e1,e2,e3) =
			match e3 with
			|X -> (mkfloat e1)
			|INT a -> float(a)
			|REAL a -> a
			|ADD(a,b) -> calc(e1,e2,a) +. calc(e1,e2,b)
			|SUB(a,b) -> calc(e1,e2,a) -. calc(e1,e2,b)
			|MUL(a,b) -> calc(e1,e2,a) *. calc(e1,e2,b)
			|DIV(a,b) -> calc(e1,e2,a) /. calc(e1,e2,b)
			|SIGMA(a,b,c) -> sigmais((REAL (calc (e1,e2,a))),(REAL (calc (e1,e2,b))),c)
			|INTEGRAL(a,b,c) -> inteis((REAL (calc (e1,e2,a))),(REAL (calc (e1,e2,b))),c)

		in
		if (((mkfloat e2)-.(mkfloat e1))<0.1) then 0.0
		else
		match e3 with
		|X -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
		|ADD (a,b) -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
		|MUL (a,b) -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
		|SUB (a,b) -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
		|DIV (a,b) -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
		|SIGMA (a,b,c) -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
		|INTEGRAL (a,b,c) -> calc(e1,e2,e3)*.(0.1) +. inteis(REAL((mkfloat e1)+.0.1),e2,e3)
	in	
	match exp with
	|INT a -> float(a)
	|REAL a -> a
	|ADD (a,b) -> addis(a,b)
	|SUB (a,b) -> subis(a,b)
	|MUL (a,b) -> mulis(a,b)
	|DIV (a,b) -> divis(a,b)
	|SIGMA (a,b,c) -> sigmais(a,b,c)
	|INTEGRAL(a,b,c) -> if ((mkfloat a)>(mkfloat b)) then -.inteis(b,a,c)
			    else inteis(a,b,c)
	|X -> raise FreeVariable
