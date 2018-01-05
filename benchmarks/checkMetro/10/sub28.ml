open String

type metro = STATION of name
           | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

let checkMetro m =
 (* contain_id : list name -> name -> bool *)
 let rec contain_id store id = 
  match store with 
   h::t -> (if (h=id)
   	    then (true)
	    else (contain_id t id)
   	   )
   |[] -> false
 in

 (* add_id : list name -> id -> list name *)
 let rec add_id store id = 
  if (contain_id store id)
  then (store)
  else (id::store)
 in

 (* checkMetro_sub : metro -> list name -> bool  - 작업을 위한 주요 함수 *)
 let rec checkMetro_sub m_sub store = 
  match m_sub with
   STATION id -> (contain_id store id)
   |AREA (id, m1) -> (checkMetro_sub m1 (add_id store id))
   |CONNECT (m1, m2) -> ((checkMetro_sub m1 store) &
   			 (checkMetro_sub m2 store))
 in

 (* 본 함수 시작 *)
 (checkMetro_sub m [])
;;

(* test code *)
let t1 = AREA("a", STATION "a");;
let t2 = AREA("a", AREA("a", STATION "a"));;
let t3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")));;
let t4 = AREA("a", CONNECT(STATION "a", AREA("b",STATION "a")));;

let f1 = AREA("a", STATION "b");;
let f2 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")));;
let f3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")));;
