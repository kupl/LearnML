type metro = STATION of name
  |AREA of name * metro
  |CONNECT of metro * metro
and name = string
let rec checkcorrect l =
  match l with
  |([],station) -> false
  |(a::lst,station) -> if a=station then true else checkcorrect (lst,station)
let rec checkMetro2 m =
  match m with
  |(lst,AREA(left,right))->checkMetro2 (left::lst,right)
  |(lst,STATION(station))->checkcorrect (lst,station)
  |(lst,CONNECT(left,right))->checkMetro2 (lst,left) && checkMetro2 (lst,right)
let checkMetro n = checkMetro2 ([],n)
