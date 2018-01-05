type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string;;

(* arrayset util *) 

(* arr = {} *)
let arrayset_empty (arr) =
  (Array.length(arr) = 0) ;;

(* arr =) value *)
let arrayset_contains (arr, value) =
  let rec contains i =
    match i with
    | 0 -> false
    | _ -> (arr.(i-1) = value) || contains(i-1) in
  contains(Array.length(arr)) ;;
(* arr U {value} *)
let arrayset_add (arr, value) =
  if arrayset_contains(arr, value) then arr else (Array.append arr [|value|]) ;;

(* arr_a U arr_b *)
let arrayset_union (arr_a, arr_b) =
  let rec union b =
    match b with
    | 0 -> arr_a
    | _ -> arrayset_add(union(b-1), arr_b.(b-1)) in
  union(Array.length(arr_b));;
(* arr_a - arr_b *)
let arrayset_difference (arr_a, arr_b) =
  let rec difference a =
    match a with
    | 0 -> [||] 
    | _ ->
      let pred_set =
        difference(a-1)
      and
      appender =
        let tail_a = arr_a.(a-1) in
          if arrayset_contains(arr_b, tail_a) then [||] else [|tail_a|]
      in
      Array.append pred_set appender
  in
  difference(Array.length(arr_a));;

let checkMetro checkee =
  let rec get_bad_metro_set metro =
    match metro with
    | STATION name -> [|name|]
    | AREA (name, inner_metro) -> arrayset_difference(get_bad_metro_set(inner_metro), [|name|])
    | CONNECT (inner_metro_a, inner_metro_b) -> arrayset_union(get_bad_metro_set(inner_metro_a), get_bad_metro_set(inner_metro_b)) in
  arrayset_empty(get_bad_metro_set(checkee)) ;;

(* test cases *)
assert(checkMetro(AREA("a", STATION "a"))) ;;
assert(checkMetro(AREA("a", AREA("a", STATION "a")))) ;;
assert(checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))) ;;
assert(checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));;

assert(not(checkMetro(AREA("a", STATION "b"))));;
assert(not(checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))));;
assert(not(checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))));;
assert(not(checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))));;
