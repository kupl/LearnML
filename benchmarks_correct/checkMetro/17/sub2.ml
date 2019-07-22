(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-13
  Homework-# : 2-4
  Excercise-Name : Check metro map
*)

type metro = STATION of name
          | AREA of name * metro
          | CONNECT of metro * metro
and name = string

let checkMetro metro = (
  let rec checkMetroInContext (metro, ctx) = (
    match metro with
    | CONNECT (m1,m2) -> (checkMetroInContext (m1, ctx)) && (checkMetroInContext (m2,ctx))
    | AREA (name, m) -> (
      checkMetroInContext(m, name::ctx)
    )
    | STATION name -> (
      List.mem name ctx
    )
  ) in checkMetroInContext (metro, [])
)

