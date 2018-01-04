type env = (string * int) list

let rec find_env : env -> string -> int
= fun env x ->
  match env with
 | [] -> raise (Failure (x ^ "Not Found"))
 | (y, v)::tl -> if (y = x) then v else find_env tl x

let rec sub_grading_diff : ae -> (string * int) list -> int
= fun aexp env ->
  match aexp with
  | CONST n -> n
  | VAR x -> find_env env x
  | POWER (x, n) -> 
    if n = 0 then 1 else (find_env env x) * (sub_grading_diff (POWER (x, n-1)) env)
  | TIMES l ->
    begin
      match l with
      | [] -> raise (Failure "Invalid")
      | [hd] -> sub_grading_diff hd env
      | hd::tl -> (sub_grading_diff hd env) * (sub_grading_diff (TIMES tl) env)
    end
  | SUM l ->
    begin
      match l with
      | [] -> raise (Failure "Invalid")
      | [hd] -> sub_grading_diff hd env
      | hd::tl -> (sub_grading_diff hd env) + (sub_grading_diff (SUM tl) env)
    end

let rec grading_diff : (ae * string) -> (string * int) list -> int
= fun (aexp,str) env -> sub_grading_diff (diff (aexp,str)) env
