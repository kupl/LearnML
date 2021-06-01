type lambda = V of var
        | P of var * lambda
        | C of lambda * lambda
    and var = string

let check met =
    let rec checkInclude m l =
        match m with
        | V s -> List.mem s l
        | P (a, inner_m) -> checkInclude inner_m (a::l)
        | C (a, b) -> (checkInclude a l) && (checkInclude b l)
    in checkInclude met [];;

(*
let bool_to_string e =
    match e with
    | true -> "true"
    | false -> "false";;

print_endline( bool_to_string( check( P("a", V "a") ) ) );;
print_endline( bool_to_string( check( P("a", P("a", V "a") ) ) ) );;
print_endline( bool_to_string( check( P("a", P("b", C(V "a", V "b")) ) ) ) );;
print_endline( bool_to_string( check( P("a", C(V "a", P("b", V "b") ) ) ) ) );;
print_endline( bool_to_string( check( P("a", V "b") ) ) );;
print_endline( bool_to_string( check( P("a", C(V "a", P("b", V "c"))) ) ) );;
print_endline( bool_to_string( check( P("a", P("b", C(V "a", V "c"))) ) ) );;
*)
