module type ZEXPR =
	sig
		exception Error of string
		type id = string
		type expr = NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list
			| VAR of id
			| LET of id * expr * expr
		type environment
		type value
		val emptyEnv: environment
		val eval: environment * expr -> value
		val print_value: value -> unit
	end

module Zexpr: ZEXPR =
    struct
        exception Error of string
        type id = string
        type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr
        type environment = EMPTY
			| SOME of id * expr
        type value = int

        let emptyEnv: environment = EMPTY

        let rec evalIter: environment list * expr -> value = fun (len, ex) ->
			match ex with
			NUM n -> n
			| PLUS (a, b) -> evalIter (len, a) + evalIter (len, b)
			| MINUS (a, b) -> evalIter (len, a) - evalIter (len, b)
			| MULT (a, b) -> evalIter (len, a) * evalIter (len, b)
			| DIVIDE (a, b) -> evalIter (len, a) / evalIter (len, b)
			| MAX lex -> getMax (len, lex)
			| VAR str -> evalIter (len, searchEnlist (len, str))
			| LET (str, a, b) -> evalIter (replaceOrAdd (len, SOME (str, a)), b)
		and getMax: environment list * expr list -> value = fun (len, lex) ->
			match lex with
			[] -> 0
			|_ -> getMaxIter (len, lex, -2147483648)
		and getMaxIter: environment list * expr list * value -> value = fun (len, lex, max) ->
			match lex with
			[] -> max
			| hd::tl ->
				if (evalIter (len, hd) > max) then getMaxIter (len, tl, evalIter (len, hd))
				else getMaxIter (len, tl, max)
		and searchEnlist: environment list * id -> expr = fun (len, i) ->
			match len with
			[] -> raise (Error "FreeVariable")
			| hd::tl ->
				match hd with
				EMPTY -> raise (Error "FreeVariable")
				|SOME (a, b) ->
					if (0 == String.compare a i) then b
					else searchEnlist (tl, i)

		and replaceOrAdd: environment list * environment -> environment list = fun (len, en) ->
			match (len, en) with
			([], _) -> [en]
			|(SOME(a, b)::tl, SOME(c, d)) ->
				if (0 == String.compare a c) then SOME(a, d)::tl
				else SOME(a, b)::replaceOrAdd (tl, en)
			|(_, _) -> raise (Error "FreeVariable")

        let eval: environment * expr -> value = fun (en, ex) ->
            match en with
			EMPTY -> evalIter ([], ex)
			| _ -> evalIter ([en], ex)

        let print_value: value -> unit = fun (v) -> Printf.printf "%d" v
    end
