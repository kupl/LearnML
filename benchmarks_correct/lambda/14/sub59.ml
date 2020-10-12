type lambda =
    V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let rec checker var_list m = match m with
    | V s ->
            let p str =
                s = str in
            List.exists p var_list
    | P (a, m) ->
            checker (a::var_list) m
    | C (m1, m2) ->
            checker var_list m1 && checker var_list m2
;;

let check m = checker [] m;;
