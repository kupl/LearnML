type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec dec met list=
  match met with
  |STATION a -> if(List.mem a list)=true then true else false
  |CONNECT (m1, m2) -> (dec m1 list)&&(dec m1 list)
  |AREA(n1, m1)->(dec m1 (n1::list))
  
  let rec checkMetro m=
  match m with
  |STATION a -> false
  |AREA(n1, m1) -> (dec m (n1::[]))  
  |CONNECT(m1, m2) ->(dec m1 [])&&(dec m2 [])
