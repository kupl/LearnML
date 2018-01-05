type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
            and name = string

let checkMetro (m: metro) : bool =
  let rec op ((l: string list), (a: metro)) =
    match a with
    | STATION n -> List.exists (fun x -> x = n) l
    | AREA (n, a') -> op((List.append [n] l), a')
    | CONNECT (a', a'') -> (op (l, a')) && op (l, a'')
  in 
  op ([], m)