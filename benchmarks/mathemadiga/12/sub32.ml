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

let rec mathemadiga e =

let rec sigma down up e sum =

let rec change e a =
match e with
| INT n -> float_of_int n
| X -> a
| REAL r -> r
| ADD (e1, e2) -> (change e1 a) +. (change e2 a)
| SUB (e1, e2) -> (change e1 a) -. (change e2 a)
| MUL (e1, e2) -> (change e1 a) *. (change e2 a)
| DIV (e1, e2) -> (change e1 a) /. (change e2 a)
| SIGMA (e1, e2, e3) -> mathemadiga e
| INTEGRAL (e1, e2, e3) -> mathemadiga (INTEGRAL (REAL (change e1 a), REAL (change e2 a), e3))

in

if (down>up) then sum else (sigma (1 + down) up e (sum +. (change e (float_of_int down))))

 
in



let rec integral down up e sum =

let rec change e a =
match e with
| INT n -> float_of_int n
| X -> a
| REAL r -> r
| ADD (e1, e2) -> (change e1 a) +. (change e2 a)
| SUB (e1, e2) -> (change e1 a) -. (change e2 a)
| MUL (e1, e2) -> (change e1 a) *. (change e2 a)
| DIV (e1, e2) -> (change e1 a) /. (change e2 a)
| SIGMA (e1, e2, e3) -> mathemadiga e
| INTEGRAL (e1, e2, e3) -> mathemadiga (INTEGRAL (REAL (change e1 a), REAL (change e2 a), e3))


in

if ((down +. 0.1)>up) then (sum +. ((up -. down) *. (change e down))) else (integral (0.1 +. down) up e (sum +. (0.1 *. (change e down))))

in

let rec changesi e =
match e with
| INT n -> float_of_int n
| X -> raise InvalidSigma
| REAL r -> raise InvalidSigma
| ADD (e1, e2) -> (changesi e1) +. (changesi e2)
| SUB (e1, e2) -> (changesi e1) -. (changesi e2)
| MUL (e1, e2) -> (changesi e1) *. (changesi e2)
| DIV (e1, e2) -> (changesi e1) /. (changesi e2)
| SIGMA (e1, e2, e3) -> mathemadiga e
| INTEGRAL (e1, e2, e3) -> mathemadiga e
in

let rec changein e =
match e with
| INT n -> float_of_int n
| X -> raise FreeVariable
| REAL r -> r
| ADD (e1, e2) -> (changein e1) +. (changein e2)
| SUB (e1, e2) -> (changein e1) -. (changein e2)
| MUL (e1, e2) -> (changein e1) *. (changein e2)
| DIV (e1, e2) -> (changein e1) /. (changein e2)
| SIGMA (e1, e2, e3) -> mathemadiga e
| INTEGRAL (e1, e2, e3) -> mathemadiga e
in


match e with
| INT n -> float_of_int n
| REAL n -> n
| X -> raise FreeVariable
| ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
| SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
| MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
| DIV (e1, e2) -> (mathemadiga e1) /. (mathemadiga e2)
| SIGMA (e1, e2, e3) -> (match (e1,e2,e3) with
			| (INT e1, INT e2, e3) -> sigma e1 e2 e3 0.0
			| (INT e1, REAL e2, e3) -> sigma e1 (int_of_float e2) e3 0.0
			| (REAL e1, INT e2, e3) -> sigma (int_of_float e1) e2 e3 0.0
			| (REAL e1, REAL e2, e3) -> sigma (int_of_float e1) (int_of_float e2) e3 0.0
			| _ -> raise InvalidSigma)
(*			if((changesi e1)>(changesi e2)) then 0.0 else sigma (changesi e1) (changesi e2) e3 0.0*)
| INTEGRAL (e1, e2, e3) -> if(mathemadiga e1>mathemadiga e2) 
			then 0.0 -. (integral (mathemadiga e2) (mathemadiga e1) e3 0.0) 
			else integral (mathemadiga e1) (mathemadiga e2) e3 0.0

;;
