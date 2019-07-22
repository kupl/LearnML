type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = fun m ->
  let rec list_check : string list * string -> bool = fun (l, s) ->
    match l with
    | [] -> false
    | head :: tail ->
      if(head = s) then true
      else list_check(tail, s) in
  let rec r_checkmetro : metro * string list -> bool = fun (m, l) ->
    match m with
    | STATION name -> list_check(l, name)
    | AREA (name, metro) -> r_checkmetro(metro, name :: l)
    | CONNECT (metro1, metro2) -> r_checkmetro(metro1, l) && r_checkmetro(metro2, l) in
  r_checkmetro(m, [])
