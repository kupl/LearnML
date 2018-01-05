type metro =
    STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let rec checker name_list m = match m with
    | STATION s ->
            let p str =
                s = str in
            List.exists p name_list
    | AREA (a, m) ->
            checker (a::name_list) m
    | CONNECT (m1, m2) ->
            checker name_list m1 && checker name_list m2
;;

let checkMetro m = checker [] m;;
