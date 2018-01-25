(* 2011210039 Kang Seungwoo *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec weight(branch) = match branch with
  CompoundBranch(len, mob) -> (match mob with (lb,rb) -> (weight(lb) + weight(rb)))
     | SimpleBranch(len, wei) -> wei

  let tot_weight(lb, rb) = weight(lb) + weight(rb)

  let rec balanced (lb, rb)= match lb with 
  CompoundBranch(l1, m1) -> balanced m1 && (match rb with
	| CompoundBranch(l3, m3) -> balanced m3
	| SimpleBranch(l4, w4) -> if l1*(tot_weight m1) = l4*w4 then true else false)
    | SimpleBranch(l2, w2) -> (match rb with
	| CompoundBranch(l3, m3) -> balanced m3 && (if l3*(tot_weight m3) = l2*w2 then true else false)
	| SimpleBranch(l4, w4) -> (if l2*w2 = l4*w4 then true else false))
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string  

  let check e = 
  let vars = ref [] in
  let rec chk e1 =
  (match e1 with
  V v -> List.mem v !vars
    | P (v,e1) -> (vars:=!vars@[v] ; chk e1)
    | C (e1,e2) -> (chk e1 && chk e2))in chk e
end

(***********************************)
(**            Problem 3          **)
(***********************************)
module Problem3 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string

  type value = Int of int | Bool of bool 
           | Procedure of var * exp * env 
           | RecProcedure of var * var * exp * env
  and env = var -> value

  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x

  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  = fun op e1 e2 env ->
	let v1 = eval e1 env in
	let v2 = eval e2 env in
		(match v1,v2 with
		| Int n1, Int n2 -> Int (op n1 n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))
  and eval: exp -> env -> value
  =fun exp env -> match exp with
     | CONST n -> Int n
     | VAR x -> apply_env env x
     | ADD (e1,e2) -> eval_bop (+) e1 e2 env
     | SUB (e1,e2) -> eval_bop (-) e1 e2 env
     | ISZERO e -> (match eval e env with
	| Int n -> if n = 0 then Bool true else Bool false
	| _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
     | IF (e1,e2,e3) -> (match eval e1 env with
	| Bool true -> eval e2 env
	| Bool false -> eval e3 env
	| _ -> raise (Failure "Type Error: condition must be Bool type"))
     | LET (x,e1,e2) ->
	let v1 = eval e1 env in
	eval e2 (extend_env (x, v1) env)
     | LETREC (f,x,e1,e2) -> 
	eval e2 (extend_env (f, RecProcedure(f,x,e1,env)) env)
     | PROC (x,e) -> Procedure(x,e,env)
     | CALL (e1,e2) -> match eval e1 env with
	| Procedure(x,ef,env2) -> eval ef (extend_env (x, (eval e2 env)) env2)
	| RecProcedure(f,x,ef,env2) -> eval ef (extend_env (x,(eval e2 env)) (extend_env (f, RecProcedure(f,x,ef,env2)) env2))
	| _ -> raise (Failure "Error: Given E1 is neither Procedure nor RecProcedure!")

(*
�� ��ü�� �巯���ִ� value�� ����ϴ� ���, (�� ��쿣 LETREC�� CALL�� f->(f,x,E,��) �������� �巯���ִ� value�� ����Ͽ���.)
explicit�ϰ� RecProcedure(f,x,ef,env2) ��� ��ø� ������ ������ 
	This expression has type env = var -> value
	but an expression was expected of type var -> var * var * exp * env
	Type value is not compatible with type var * var * exp * env
�̷� ������ ���Ƿ� ����
*)

  let run: program -> value
  =fun pgm -> eval pgm empty_env
end

(***********************************)
(**            Problem 4          **)
(***********************************)

module Problem4 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
  
  type nl_program = nl_exp
  and nl_exp = 
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp

  let rec nth l n =
  match l with
    | hd::tl -> if hd=n then 0 else 1 + nth tl n
    | [] -> raise (Failure "Error: given expression contains free variable!")

  let translate : program -> nl_program = fun pgm ->
  let envr = ref [] in
  let rec trans pgm1 =
  (match pgm1 with
    | CONST n -> NL_CONST n
    | VAR v -> NL_VAR (nth !envr v)
    | ADD (e1,e2) -> NL_ADD (trans e1,trans e2)
    | SUB (e1,e2) -> NL_SUB (trans e1,trans e2)
    | ISZERO e -> NL_ISZERO (trans e)
    | IF (e1,e2,e3) -> NL_IF (trans e1,trans e2,trans e3)
    | LET (x,e1,e2) -> (match e1 with 
	| CONST v1 -> (envr:=[x] @ !envr) ; NL_LET (trans e1,trans e2)
	| _ -> (fun x -> (match x with
		| NL_LET(e11,e22) -> NL_LET(e22,e11)
		| _ -> raise (Failure "This never happens!"))) (NL_LET (((envr:=[x] @ !envr) ; (trans e2)),trans e1)))
    | PROC (x,e) -> (envr:=[x] @ !envr ; NL_PROC (trans e))
    | CALL (e1,e2) -> NL_CALL (trans e1,trans e2)) in trans pgm
(*

let �κ��� �������� ������ �Ǿ��°�?
: ���� �ڵ�� ������ ���Ҵ�.
| CONST v1 -> (envr:=[x] @ !envr) ; NL_LET (trans e1,trans e2)
| _ -> (trans e1, ((envr:=[x] @ !envr) ; (trans e2)))

�ǵ��� e1�� CONST�� �ƴ� ��� trans e1�� ����� ��ģ �� environment�� Ȯ���ϰ�, �� Ȯ��� environment���� e2�� ����϶�� ���̾���.
������ �־��� ������ �� �ڵ�� NL_SUB (NL_VAR 1, NL_VAR 2) ����� ���´�.
�� ������ �м��غ� ��� ,�� ����� �� expression�� ��� �ڿ����� ���� �����Ѵٴ� ����� Ȯ���� �� �־���.
�� �� �ڵ�� �ǵ��ʹ� �ٸ��� environment Ȯ�� �ϰ�, trans e2�� ����ϰ�, �� ���� trans e1�� ����ϴ�, CONST v1 -> �ڿ� �ִ� �ڵ�� ��ǻ� �ٸ��� ���� �ڵ忴�� ���̴�.

�׷��� �� ���� ������ �ٲ� �ʿ䰡 �־���, ��������� NL_LET (((envr:=[x] @ !envr) ; (trans e2)),trans e1) �� �Ǿ���.
�׷��� �̰��� ���ϴ� ����� �ƴϴ�. ������ nameless function�� �����Ͽ� �� �� expression ��ġ�� �ٲٴ� �Լ��� �ۼ��Ͽ���.
�� nameless function�� (match x with NL_LET(e11,e22) -> NL_LET(e22,e11) | _ -> raise (Failure "This never happens")) �̴�.
_ -> �κ��� ��� �ʿ� ���µ� �׳� warning ���� �Ⱦ ����־���.

*)
end

(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list

  let nl_empty_env = []
  let nl_extend_env v e = v::e
  let rec nl_apply_env e x = match e with
	| hd::tl -> if x=0 then hd else (nl_apply_env tl (x-1))
	| [] -> raise (Failure "i don't know")

  let rec nl_eval_bop op e1 e2 nl_env =
	let v1 = nl_eval e1 nl_env in
	let v2 = nl_eval e2 nl_env in
		(match v1,v2 with
		| NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))
  and nl_eval: nl_exp -> nl_env -> nl_value
  =fun nl_exp nl_env -> match nl_exp with
     | NL_CONST n -> NL_Int n
     | NL_VAR x -> nl_apply_env nl_env x
     | NL_ADD (e1,e2) -> nl_eval_bop (+) e1 e2 nl_env
     | NL_SUB (e1,e2) -> nl_eval_bop (-) e1 e2 nl_env
     | NL_ISZERO e -> (match nl_eval e nl_env with
	| NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
	| _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
     | NL_IF (e1,e2,e3) -> (match nl_eval e1 nl_env with
	| NL_Bool true -> nl_eval e2 nl_env
	| NL_Bool false -> nl_eval e3 nl_env
	| _ -> raise (Failure "Type Error: condition must be Bool type"))
     | NL_LET (e1,e2) ->
	let v1 = nl_eval e1 nl_env in
	nl_eval e2 (nl_extend_env v1 nl_env)
     | NL_PROC (x) -> NL_Procedure(x,nl_env)
     | NL_CALL (e1,e2) -> match nl_eval e1 nl_env with
	| NL_Procedure(ef,env2) -> nl_eval ef (nl_extend_env (nl_eval e2 nl_env) env2)
	| _ -> raise (Failure "Error: Given E1 is not Procedure!")

  let nl_run : nl_program -> nl_value = fun pgm -> nl_eval pgm nl_empty_env
end
