type metro = 
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and 
  name = string
(*
let rec checkMetro (m: metro) : bool =
  match m with
  | STATION n -> true
  | CONNECT (m1, m2) -> (checkMetro m1) && (checkMetro m2)
  | AREA (n, m) -> 
      ( match m with
      | STATION n2 -> (if (n=n2) then true else false)
      | _ -> (checkMetro m)
      )
*)
let rec checkMemory (m, mem): bool =
  match mem with 
  | [] -> false
  | hd::tl -> 
      if (hd=m)
      then true
      else checkMemory(m, tl)

let rec checkMetro_wMem (m, mem): bool=
  match m with
  | STATION n -> checkMemory(n, mem)
  | CONNECT (m1, m2)-> (checkMetro_wMem (m1, mem)) && (checkMetro_wMem (m2, mem))
  | AREA (n, m1) -> checkMetro_wMem (m1, (n::mem))

let checkMetro (m: metro) : bool=
  checkMetro_wMem(m, [])
