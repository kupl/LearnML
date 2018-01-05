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
  type environment = (string * int) list(* variable, binded value *)
  type value = int
  let emptyEnv: environment = []
  let rec eval (env , e) : value = match e with (* env*expr -> value *)
    | NUM n -> n
    | PLUS (e1, e2) -> eval(env, e1) + eval(env, e2)
    | MINUS (e1, e2) -> eval(env, e1) - eval(env, e2)
    | MULT (e1, e2) -> eval(env, e1) * eval(env, e2)
    | DIVIDE (e1, e2) -> eval(env, e1) / eval(env, e2) (*integer division ! *)
    | MAX elist -> ( (** Error! *)
      let rec aux acc elist : value = match elist with (* value -> expr list -> value *)
        | [] -> acc
        | hd :: tl -> (
            let hd_val = eval(env,hd) in
            if acc < hd_val then aux hd_val tl
            else aux acc tl
          )
      in

      match elist with
       | [] -> 0
       | hd :: tl -> aux (eval(env,hd)) tl
      )
    | VAR x -> (
        let rec getFirst(x, env) : value option = match env with
          | (y, y_val) :: tl -> (
              if (x=y) then Some y_val
              else getFirst(x, tl)
            )
          | [] -> None (*raise Error("no such variable binded!") *)
        in

        let x_valoption = getFirst(x, env) in

        match x_valoption with
          | None -> raise (Error "FreeVariable") (* "no such variable binded!" *)
          | Some x_val -> x_val
      )
    | LET (x, x_e, e1) -> eval((x, eval(env,x_e)) :: env, e1)
  let print_value v = print_int v (* value -> unit *)
end

(** Testcases *)
(**
let _ = Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.NUM 1)));;

let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x")) in
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x")))) in
Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, e1));;

Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [])));;

Zexpr.print_value(Zexpr.eval(Zexpr.emptyEnv, (Zexpr.MAX [Zexpr.NUM (-1)])));;
*)
