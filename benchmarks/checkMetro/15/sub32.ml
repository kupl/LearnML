type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

module NameSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = name
  end
)

let checkMetro target =
  let rec checkWithSet target set =
    match target with
      STATION name -> NameSet.mem name set
    | AREA (name, subMet) -> checkWithSet subMet @@ NameSet.add name set
    | CONNECT (subMet1, subMet2) ->
      checkWithSet subMet1 set && checkWithSet subMet2 set
  in
  checkWithSet target NameSet.empty
