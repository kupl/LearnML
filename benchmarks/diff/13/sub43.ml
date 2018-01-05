type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec diff(ae, str) =
    match ae with
    | CONST n -> CONST 0
    | VAR s -> if s = str then CONST 1
               else CONST 0
    | POWER (s, n) -> if s = str then TIMES [CONST n; POWER(s, n-1)]
                      else CONST 0
    | SUM [] -> raise InvalidArgument
    | TIMES [] -> raise InvalidArgument
    | SUM el -> SUM (List.map (fun x -> diff(x, str)) el)
    | TIMES el -> SUM (List.map (fun x -> TIMES (diff(x, str)::(List.filter (fun y -> x <> y) el))) el)
