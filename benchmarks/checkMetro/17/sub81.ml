(* 2014-17189 이소희
 * Exercise 2-4, Due: 9/28, 24:00 *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

type name_list = string list
type metroL = STATION of name
              | AREA of name_list * metroL
              | CONNECT of metroL * metroL

let rec makeMetroL (m : metro) : metroL = 
  match m with
    | STATION name -> STATION name
    | AREA (name, m1) -> (match m1 with
                            STATION name1 -> AREA (name::[], STATION name1)
                          | AREA (name1, m2)
                              -> AREA (name::name1::[], makeMetroL(m2))
                          | CONNECT (m2, m3)
                              -> AREA (name::[], makeMetroL(CONNECT (m2, m3))))
    | CONNECT (m1, m2) -> CONNECT (makeMetroL(m1), makeMetroL(m2))
 
let rec checkMetroL (m : metroL) : bool =
  match m with
    | STATION name -> false
    | AREA (name_list, m1) -> (match m1 with
                                 STATION name1 -> (List.mem name1 name_list)
                               | AREA (name_list1, m2) -> checkMetroL(AREA (List.append name_list name_list1, m2))
                               | CONNECT (m2, m3) 
                                   -> checkMetroL(AREA (name_list, m2)) && checkMetroL(AREA (name_list, m3))) 
    | CONNECT (m1, m2) -> checkMetroL(m1) && checkMetroL(m2)

let rec checkMetro (m : metro) : bool = checkMetroL(makeMetroL(m)) 

(* test : test function *)
(*
let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case(1, true == checkMetro(AREA("a", STATION "a"))); 
  test_case(2, true == checkMetro(AREA("a", AREA("a", STATION "a")))); 
  test_case(3, true == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))); 
  test_case(4, true == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))); 
  test_case(5, false == checkMetro(AREA("a", STATION "b"))); 
  test_case(6, false == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))); 
  test_case(7, false == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))); 
  test_case(8, true == checkMetro(CONNECT(AREA("a", STATION "a"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
  test_case(9, false == checkMetro(CONNECT(AREA("c", STATION "c"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
  test_case(10, false == checkMetro(STATION "a"))
*)

