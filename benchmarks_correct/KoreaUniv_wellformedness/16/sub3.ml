
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec find_match : string * string list -> bool
= fun(vr,vr_list) -> 
	match vr_list with
	| [] -> false
	| hd::tl-> if vr = hd then true else find_match(vr,tl) (*list check*)

let rec vr_match : lambda * string list -> bool
= fun(lambda,vr_list) -> 
	match lambda with 
	|V(vr)-> find_match(vr,vr_list)
	|P(vr,e1)->vr_match(e1,vr::vr_list)
	|C(e1,e2)->if (vr_match(e1,vr_list)&&vr_match(e2,vr_list)) then true else false


  let check : lambda -> bool
  = fun lambda -> 
  	match lambda with
  	| V(vr) -> false
  	| P(vr,e1)-> vr_match(e1,[vr])
   	| C(e1,e2) -> (vr_match(e1,[]) && vr_match(e2,[]))
