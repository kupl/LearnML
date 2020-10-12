(*HW2-Exercise 4*)
type lambda = V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let check met =
    let rec checkId met id_list =
        match met with
        | V(var) -> List.mem var id_list
        | P(var, met0) -> checkId met0 (var::id_list)
        | C(met1, met2) -> ((checkId met1 id_list) && (checkId met2 id_list))
    in
    checkId met []
