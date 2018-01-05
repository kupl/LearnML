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

  type environment =(id*int) list
  type value=int

  let emptyEnv : environment=[]
  let rec eval : environment * expr -> value=fun(env,exp)->
    let rec findId:environment*id->int=fun(fenv,fid)->
      match fenv with
      |[]->raise(Error "FreeVariable")
      |(eid,ev)::tl->if eid=fid then ev else findId(tl,fid)
    in
      match exp with
      |NUM a->a
      |PLUS (a,b)->eval(env,a)+eval(env,b)
      |MINUS (a,b)->eval(env,a)-eval(env,b)
      |MULT (a,b)->eval(env,a)*eval(env,b)
      |DIVIDE (a,b)->eval(env,a)/eval(env,b)
      |MAX s->(match s with
        |[]->0
        |hd::[]->eval(env,hd)
        |hd::dhd::dtl->if eval(env,hd)>eval(env,MAX(dhd::dtl)) then eval(env,hd) else eval(env,MAX(dhd::dtl))
       )
      |VAR a->findId(env,a)
      |LET (a,b,c)->eval((a,eval(env,b))::env,c)


  let print_value : value -> unit=fun(a)->
    print_endline(string_of_int(a))

end

