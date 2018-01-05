(* hw2-2, 2012-11259 *)

type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string
and names = name list

let rec insert: name -> names -> names =
  fun n ns ->
    match ns with
	| [] -> []
	| hd::tl ->
      if n > hd then hd::(insert n tl)
	  else if n = hd then ns 
	  else n::ns

let rec append: names -> names -> names =
  fun ns1 ns2 ->
    match (ns1, ns2) with
	| ([], ns) -> ns
	| (ns, []) -> ns
    | (h1::t1, h2::t2) ->
	  if h1 < h2 then h1::(append t1 ns2)
	  else if h1 > h2 then h2::(append ns1 t2)
	  else h1::(append t1 t2)

let rec remove: name -> names -> names = 
  fun n ns ->
    match ns with
	| [] -> []
	| hd::tl ->
	  if n > hd then hd::(remove n tl)
	  else if n = hd then tl
	  else ns

let rec stationNames: metro -> names =
  fun m ->
    match m with
	| STATION n -> n::[]
	| AREA (n, m) -> remove n (stationNames m)
	| CONNECT (m1, m2) -> append (stationNames m1) (stationNames m2)

let rec checkMetro: metro -> bool =
  fun m ->
    if (stationNames m) = [] then true
	else false

