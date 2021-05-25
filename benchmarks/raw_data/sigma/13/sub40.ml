(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

let rec sigma (a, b, f) =
    if a > b then 0
    else f(a) + sigma((a+1), b, f)

