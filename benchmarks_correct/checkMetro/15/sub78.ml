type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let rec remainStation : metro -> string list = fun m ->
        match m with
        |STATION(name) -> [name]
        |AREA(name,metro) ->
                let f : string -> bool = fun i -> (i<>name) in
                let rm = remainStation(metro) in
                List.filter f rm
        |CONNECT(metro1,metro2) -> remainStation(metro1)@remainStation(metro2)

let checkMetro : metro -> bool = fun m ->
        match remainStation(m) with
        |[] -> true
        |head::tail -> false
(*
let a81 = checkMetro (AREA("a", STATION "a")) 
let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) 
let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) 
let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) 

let a85 = checkMetro (AREA("a", STATION "b")) 
let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) 
let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) 
let a88 = checkMetro (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "b"))))

let _ = print_endline(string_of_bool a81)
let _ = print_endline(string_of_bool a82)
let _ = print_endline(string_of_bool a83)
let _ = print_endline(string_of_bool a84)
let _ = print_endline(string_of_bool a85)
let _ = print_endline(string_of_bool a86)
let _ = print_endline(string_of_bool a87)
let _ = print_endline(string_of_bool a88)


let t1 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let t2 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let t3 = CONNECT( AREA ("a", STATION "a"), AREA ("b", STATION "b"))
let t4 = AREA("a", AREA("b", AREA("c", AREA("d", AREA("e", STATION "a")))))
let t5 = AREA("a", AREA("b", AREA("c", AREA("d", AREA("e", STATION "c")))))
let t6 = AREA("a", AREA("a", STATION "a"))
let t7 = CONNECT( CONNECT( AREA( "a", STATION "a"), AREA("b", STATION "b")),CONNECT( AREA( "a", STATION "a"), AREA("b", STATION "b")))
let t8 = AREA("a", STATION "a")

let tc1 = checkMetro t1
let tc2 = checkMetro t2
let tc3 = checkMetro t3
let tc4 = checkMetro t4
let tc5 = checkMetro t5
let tc6 = checkMetro t6
let tc7 = checkMetro t7
let tc8 = checkMetro t8
let _ = assert(tc1=true)
let _ = assert(tc2=true)
let _ = assert(tc3=true)
let _ = assert(tc4=true)
let _ = assert(tc5=true)
let _ = assert(tc6=true)
let _ = assert(tc7=true)
let _ = assert(tc8=true)

let f1 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let f2 = AREA("a", AREA("c", CONNECT(STATION "a", STATION "b")))
let f3 = CONNECT( AREA ("a", STATION "a"), AREA ("b", STATION "a"))
let f4 = AREA("a", AREA("b", AREA("c", AREA("d", AREA("e", STATION "f")))))
let f5 = AREA("a", AREA("b", AREA("c", AREA("d", AREA("e", STATION "q")))))
let f6 = AREA("a", AREA("a", STATION "b"))
let f7 = CONNECT( CONNECT( AREA( "a", STATION "a"), AREA("b", STATION "f")),CONNECT( AREA( "a", STATION "a"), AREA("b", STATION "b")))
let f8 = STATION "a"

let _ = assert((checkMetro f1)=false)
let _ = assert((checkMetro f2)=false)
let _ = assert((checkMetro f3)=false)
let _ = assert((checkMetro f4)=false)
let _ = assert((checkMetro f5)=false)
let _ = assert((checkMetro f6)=false)
let _ = assert((checkMetro f7)=false)
let _ = assert((checkMetro f8)=false)*)
