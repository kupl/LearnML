    type metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
    and name = string

    let checkMetro m = 
    (
    let rec firstcheck_rec al m =
    match m with
    | AREA(name,m1) -> firstcheck_rec (name::al) m1
    | CONNECT(m1,m2) -> firstcheck_rec al m1 && firstcheck_rec al m2
    | STATION(name) -> List.mem name al
    in
    firstcheck_rec []  m
    )

(*
    let _ =
        if checkMetro(AREA("a",STATION "b")) = true then print_string("hello")
        else print_string("hhh")

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
