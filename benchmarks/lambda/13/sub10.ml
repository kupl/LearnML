(* ID : 2007-12138 *)

type lambda = V of var
	   | P of var * lambda
           | C of lambda * lambda
and var = string

(* The main idea is simple; 
    if we succeed extracting all Area var and Station var in lists,
    we can compare it and findout it is correct or not.
*)


(* My functions
   list_station  : returns a list of station var of target(lambda type)
   list_area     : returns a list of area var of target(lambda type)
   list_matching : gets two lists and check if every element in list1 is in list2. If so, return true.
*)
let rec list_station ipt =
  match ipt with
   V(a) -> a::[]
  |P(a,m) -> list_station(m)
  |C(m1,m2) -> list_station(m1) @ list_station(m2) ;;

let rec list_area ipt =
  match ipt with
   V(a) -> []
  |P(a,m) -> a::list_area(m)
  |C(m1,m2) -> list_area(m1) @ list_area(m2);;

let rec list_matching (ipt1,ipt2) = 
  match ipt1 with
   [] -> true
  | _ -> if (List.mem (List.hd(ipt1)) ipt2) then list_matching (List.tl(ipt1),ipt2)
         else false;;
(* End of my function *)

(* The idea I wrote first is simply implemented. *)
let check ipt =
  if list_matching(list_station(ipt), list_area(ipt)) && list_matching(list_area(ipt), list_station(ipt)) then true
  else false;;

