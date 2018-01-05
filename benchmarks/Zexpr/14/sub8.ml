module Zexpr =
    struct
	    exception Error of string
	    type id = string
	    type expr =
			| NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list
			| VAR of id
            | LET of id * expr * expr

        type value = int
        type element = ELE of id * value
		type environment = ENV of element list


        let emptyEnv = ENV []

        let rec eval (environment, expr) =
            match expr with
                | NUM int -> int
                | PLUS (expr1, expr2) -> (eval (environment, expr1)) + (eval (environment, expr2))
                | MINUS (expr1, expr2) -> (eval (environment, expr1)) - (eval (environment, expr2))
                | MULT (expr1, expr2) -> (eval (environment, expr1)) * (eval (environment, expr2))
                | DIVIDE (expr1, expr2) -> (eval (environment, expr1)) / (eval (environment, expr2))
                | MAX exprList ->
                    let evaluatedList = List.map (function expr -> (eval (environment, expr))) exprList in
                        let bigger a b = if a > b then a else b in
                            (function [] -> 0 | h::t -> List.fold_left bigger h t) evaluatedList
                | VAR id ->
                    let elementList = (function ENV list1 -> list1) environment in
                        let filteredElementList = List.find_all (function ELE(newId, value) -> newId = id) elementList in
                            let firstFilteredElement = (function [] -> raise (Error "no element") | h :: t -> h) filteredElementList in
                                (function ELE (_, v) -> v) firstFilteredElement
                | LET (id, expr1, expr2) ->
                    let evaluatedValue = (eval (environment, expr1)) in
                        let newElement = ELE (id, evaluatedValue) in
                            let newEnvironMent = (function ENV list -> (ENV (newElement::list))) environment in
                                (eval (newEnvironMent, expr2))

		let int_of_value value = value
    end;;

(*
print_endline(string_of_int( Zexpr.int_of_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1)))));;

let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) in
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x")))) in
print_endline(string_of_int(Zexpr.int_of_value(Zexpr.eval(Zexpr.emptyEnv, e1))));;

print_endline(string_of_int(Zexpr.int_of_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])))));;
print_endline(string_of_int(Zexpr.int_of_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])))));;
*)
