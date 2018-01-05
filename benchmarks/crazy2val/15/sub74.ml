type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2val = function (v: crazy2) ->
    match v with
    | NIL -> 0
    | ZERO p -> 
            let prev_value = crazy2val(p) in
            prev_value * 2
    | ONE p ->
            let prev_value = crazy2val(p) in
            prev_value * 2 + 1
    | MONE p -> 
            let prev_value = crazy2val(p) in
            prev_value * 2 - 1

