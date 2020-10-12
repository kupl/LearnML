type lambda = V of var
              | P of var * lambda
              | C of lambda * lambda
              and var = string;;

let rec checkLi : lambda * (var list) -> bool = fun (m,li) -> (
match m with
    V n -> (
    if( List.mem n li) then (true) else (false)
    )| P (n, mm) -> (
    let childList : var list = n::li in
    checkLi(mm, childList)
    )| C (mm1, mm2) -> (
    checkLi(mm1,li) && checkLi(mm2,li)
    )
);;

let check : lambda -> bool = fun m -> (
  checkLi(m, [])
);;

(*
let testf = fun x ->
    if( check(x)) then (print_endline "true") else (print_endline "false");;

testf(P("a", V "a"));
testf(P("a", P("a", V "a")));
testf(P("a", P("b", C(V "a", V "b"))));
testf(P("a", C(V "a", P("b", V "a"))));

testf(P("a", V "b"));
testf(P("a", C(V "a", P("b", V "c"))));
testf(P("a", P("b", C(V "a", V "c"))));
*)


