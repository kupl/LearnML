type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff (ae, s) =
	match (ae, s) with
	| (SUM[], s) -> raise InvalidArgument
	| (TIMES[], s) -> raise InvalidArgument
	| (CONST i, s) -> CONST 0
	| (VAR s1, s) -> if (s1 = s) then CONST 1
			 else CONST 0
	| (POWER (s1, a), s) -> if (s1 = s) then TIMES[CONST a; POWER (s1, a - 1)]
				else TIMES[CONST a; diff(VAR s1, s); POWER (s1, a - 1)]
	| (SUM[a; b], s) -> SUM[diff(a, s); diff(b, s)]
	| (SUM(a::sl), s) -> SUM[diff(a, s); diff(SUM(sl), s)]
	| (TIMES[a; b], s) -> SUM[TIMES[diff(a, s); b]; TIMES[a; diff(b, s)]]
	| (TIMES(a::sl), s) -> SUM[TIMES(diff(a, s)::sl);TIMES[a; diff(TIMES(sl), s)]]
