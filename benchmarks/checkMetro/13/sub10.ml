(* ID : 2007-12138 *)

type metro = STATION of name
	   | AREA of name * metro
           | CONNECT of metro * metro
and name = string

(* The main idea is simple; 
    if we succeed extracting all Area name and Station name in lists,
    we can compare it and findout it is correct or not.
*)


(* My functions
   list_station  : returns a list of station name of target(metro type)
   list_area     : returns a list of area name of target(metro type)
   list_matching : gets two lists and check if every element in list1 is in list2. If so, return true.
*)
let rec list_station ipt =
  match ipt with
   STATION(a) -> a::[]
  |AREA(a,m) -> list_station(m)
  |CONNECT(m1,m2) -> list_station(m1) @ list_station(m2) ;;

let rec list_area ipt =
  match ipt with
   STATION(a) -> []
  |AREA(a,m) -> a::list_area(m)
  |CONNECT(m1,m2) -> list_area(m1) @ list_area(m2);;

let rec list_matching (ipt1,ipt2) = 
  match ipt1 with
   [] -> true
  | _ -> if (List.mem (List.hd(ipt1)) ipt2) then list_matching (List.tl(ipt1),ipt2)
         else false;;
(* End of my function *)

(* The idea I wrote first is simply implemented. *)
let checkMetro ipt =
  if list_matching(list_station(ipt), list_area(ipt)) && list_matching(list_area(ipt), list_station(ipt)) then true
  else false;;

