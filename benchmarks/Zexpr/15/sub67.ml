(*2-7 컴공 2014-10618 이세영*)
module type ZEXPR =
    sig
        exception Error of string
        type id=string
        type expr=
            |NUM of int
            |PLUS of expr*expr
            |MINUS of expr*expr
            |MULT of expr*expr
            |DIVIDE of expr*expr
            |MAX of expr list
            |VAR of id
            |LET of id*expr*expr
        type environment
        type value
        val emptyEnv: environment
        val eval: environment*expr->value
        val print_value: value->unit
    end
module Zexpr: ZEXPR=
    struct
        exception Error of string
        type id=string
        type expr=
            |NUM of int
            |PLUS of expr*expr
            |MINUS of expr*expr
            |MULT of expr*expr
            |DIVIDE of expr*expr
            |MAX of expr list
            |VAR of id
            |LET of id*expr*expr
        type value=int
        type varv=id*value
        type environment=varv list
        let emptyEnv=[];;
        let rec findvar (env, id)=
            match env with
            |[]->raise (Error "FreeVariable")
            |t::li->match t with
                    |(x,y)->if x=id then y else findvar(li,id);;
        let rec eval (env, ex)=
            match ex with
            |NUM x->x
            |PLUS (x,y)-> eval(env,x)+eval(env,y)
            |MINUS (x,y)-> eval(env,x)-eval(env,y)
            |MULT (x,y)-> eval(env,x)*eval(env,y)
            |DIVIDE (x,y)-> eval(env,x)/eval(env,y)
            |MAX li-> let rec getMax li=
                      match li with
                      |[]->0
                      |[t]->eval (env, t)
                      |t::li->if eval(env,t)>(getMax li) then eval(env,t)
                              else getMax li
                in getMax li
            |VAR i-> findvar(env, i)
            |LET (i,x,y)-> eval((i,eval(env, x))::env, y);;
        let print_value vl=
            Printf.printf "%d\n" vl;;
    end
