module type ZEXPR = sig
    exception Error of string
    type id = string
    type expr = 
        |NUM of int
        |PLUS of expr * expr
        |MINUS of expr * expr
        |MULT of expr * expr
        |DIVIDE of expr * expr
        |MAX of expr list
        |VAR of id
        |LET of id * expr * expr

    type environment = (id,int) Hashtbl.t
    type value = int

    val emptyEnv: environment
    val eval: environment * expr -> value

    val int_of_value : value -> int
end

module Zexpr : ZEXPR = struct
    exception Error of string
    type id = string
    type expr =
        |NUM of int
        |PLUS of expr * expr
        |MINUS of expr * expr
        |MULT of expr * expr
        |DIVIDE of expr * expr
        |MAX of expr list
        |VAR of id
        |LET of id * expr * expr

    type environment = (id,int) Hashtbl.t
    type value = int

    let emptyEnv = Hashtbl.create 123456

    let eval (en,ex) =
        let max v =
            let rec submax k p =
                match k with
                    |[] -> p
                    |a::b -> if a > p then submax b a else submax b p
        in if v = [] then 0 else submax (List.tl v) (List.hd v) in

        let rec subeval (n,x) =
            match x with
                |NUM a -> a
                |PLUS (a,b) -> subeval(n,a) + subeval(n,b)
                |MINUS (a,b) -> subeval(n,a) - subeval(n,b)
                |MULT (a,b) -> subeval(n,a) * subeval(n,b)
                |DIVIDE (a,b) -> subeval(n,a) / subeval(n,b)
                |MAX a -> max (List.map (fun k -> subeval(n,k)) a)
                |VAR a -> (try Hashtbl.find n a with Not_found -> raise (Error "FreeVariable"))
                |LET (a,b,c) -> Hashtbl.add n a (subeval(n,b)); subeval (n,c)
        in subeval (emptyEnv,ex)

    let int_of_value arg = arg
end
