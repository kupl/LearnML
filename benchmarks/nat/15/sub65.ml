type nat = ZERO | SUCC of nat;;

let rec natadd ((a:nat), (b:nat))=
    match (a, b) with     
    | (ZERO, ZERO) -> ZERO
    | (ZERO, SUCC _) -> b
    | (SUCC _, ZERO) -> a
    | (SUCC l, SUCC r) -> 
            let prev = natadd(l, b) in
            SUCC (prev);;

let rec natmul = ((a:nat), (b:nat)) =
    match (a, b) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC l, SUCC r) ->
            let prev = natmul(a, r) in
            (natadd(a, prev));;

(*
let rec nat_to_int = function (n: nat) ->
    match n with
    | ZERO -> 0
    | SUCC np -> ((nat_to_int np) + 1);;
*)
