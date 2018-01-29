
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec input_var : exp -> string list -> string list = fun e lst ->match e with |V(v)->lst |P(v,e1)->(input_var e1 lst)@[v] |C(e1,e2)->(input_var e1 (input_var e2 lst)) ;;
  let rec input_exp : exp->string list -> string list = fun e lst -> match e with |V(v)->lst@[v] |P(v,e1) -> input_exp e1 lst |C(e1,e2)->input_exp e1 (input_exp e2 lst) ;;
  let rec isthere : string list -> string list -> bool = fun lst l2 -> match lst with |[]->true |hd::tl->(match l2 with |[]->false |h::t->if hd=h then true else isthere [hd] t);;
  let rec check : exp -> bool
  = fun exp -> let l2=input_var exp [] in let l1 = input_exp exp [] in match exp with |V(v) -> false |C(e1,e2)->check e1 && check e2
	|P(v,e)->( match l1 with |[]->true |hd::tl -> if isthere [hd] l2 then isthere tl l2  else false);; (* TODO *)
