type metro = STATION of name
  |AREA of name * metro
  |CONNECT of metro * metro
and name = string
let rec checkcorrect l =
  match l with
  |([],station) -> false
  |(a::list,station) -> if a=station then true else checkcorrect (list,station)
let rec checkMetro2 m =
  match m with
  |(list,AREA(left,right))->checkMetro2 (left::list,right)
  |(list,STATION(station))->checkcorrect (list,station)
  |(list,CONNECT(left,right))->checkMetro2 (list,left) && checkMetro2 (list,right)
let checkMetro n = checkMetro2 ([],n)
