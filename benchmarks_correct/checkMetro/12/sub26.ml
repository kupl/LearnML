type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro m = 
  let rec check m l = 
    match m with
    | STATION a -> List.mem a l
    | AREA(name, metro) -> (check metro (name::l)) 
    | CONNECT(m1,m2) -> (check m1 l) && (check m2 l)
  in
  check m []

  
  (*
let a = AREA("a", STATION "a")
let b = AREA("a", AREA("a", STATION "a"))
let c = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let d = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let e = AREA("a", STATION "b")
let f = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let g = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))

let l = [a;b;c;d;e;f;g]

let print_bool a =
  match a with
  | true -> print_endline "TRUE"
  | false -> print_endline "FALSE"

let _ = List.iter print_bool (List.map checkMetro l)

*)
