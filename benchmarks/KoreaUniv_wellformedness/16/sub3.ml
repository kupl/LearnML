
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec find_match : string * string list -> bool
= fun(vr,vr_list) -> 
	match vr_list with
	| [] -> false
	| hd::tl-> if vr = hd then true else find_match(vr,tl) (*list check*)

let rec vr_match : exp * string list -> bool
= fun(exp,vr_list) -> 
	match exp with 
	|V(vr)-> find_match(vr,vr_list)
	|P(vr,e1)->vr_match(e1,vr::vr_list)
	|C(e1,e2)->if (vr_match(e1,vr_list)&&vr_match(e2,vr_list)) then true else false


  let check : exp -> bool
  = fun exp -> 
  	match exp with
  	| V(vr) -> false
  	| P(vr,e1)-> vr_match(e1,[vr])
   	| C(e1,e2) -> (vr_match(e1,[]) && vr_match(e2,[]))
