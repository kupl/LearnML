module type ZEXPR = 
sig 
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

    type environment 
    type value 

    val emptyEnv : environment 
    val eval : environment * expr -> value 

    val print_value : value -> unit 
end 

module Zexpr : ZEXPR = 
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
    type environment = (id*value) list

    let emptyEnv : environment = []
    let rec isDefined ((id:id),(env:environment)) : bool =
        match env with
        | [] -> false
        | hd::tl -> (
            match hd with
            | (i,v) -> if i = id then true else isDefined(id,tl)
        )

    let rec findEnv ((id:id),(env:environment)) : value =
        match env with
        | [] -> -1
        | hd::tl -> (
            match hd with
            | (i, v) -> if i = id then v else findEnv(id,tl)
        )

    let rec deleteEnv ((id:id),(env:environment)) : environment =
        match env with
        | [] -> env
        | hd::tl -> (
            match hd with
            | (i,v) -> if i = id then tl else hd::deleteEnv(id,tl)
    )

    let rec eval ((env:environment),(ex:expr)) : value =
        match ex with
        | NUM x -> x
        | PLUS(x,y) -> eval(env,x) + eval(env,y)
        | MINUS(x,y) -> eval(env,x) - eval(env,y)
        | MULT(x,y) -> eval(env,x) * eval(env,y)
        | DIVIDE(x,y) -> eval(env,x) / eval(env,y)
        | MAX l -> (
            match l with
            | [] -> 0
            | [x] -> eval(env,x)
            | x::remain -> (
                if eval(env,x) > eval(env,MAX remain) then eval(env,x)
                else eval(env,MAX remain)
            )
        )
        | VAR x -> (
            if isDefined(x,env) then findEnv(x,env)
            else raise (Error "FreeVariable")
        )
        | LET(x,v,e) -> (
            eval((x,eval(env,v))::deleteEnv(x,env), e)
        )

    let print_value (v:value) : unit =
        print_int v
end 
