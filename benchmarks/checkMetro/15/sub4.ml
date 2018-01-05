type metro = STATION of name
              | AREA of name * metro
              | CONNECT of metro * metro
              and name = string;;

let rec checkMetroLi : metro * (name list) -> bool = fun (m,li) -> (
match m with
    STATION n -> (
    if( List.mem n li) then (true) else (false)
    )| AREA (n, mm) -> (
    let childList : name list = n::li in
    checkMetroLi(mm, childList);
    )| CONNECT (mm1, mm2) -> (
    checkMetroLi(mm1,li) && checkMetroLi(mm2,li)
    )
);;

let checkMetro : metro -> bool = fun m -> (
  checkMetroLi(m, [])
);;

(*
let testf = fun x ->
    if( checkMetro(x)) then (print_endline "true") else (print_endline "false");;

testf(AREA("a", STATION "a"));
testf(AREA("a", AREA("a", STATION "a")));
testf(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));
testf(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));

testf(AREA("a", STATION "b"));
testf(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));
testf(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))));
*)


