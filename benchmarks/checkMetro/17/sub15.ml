type metro = 
  STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro met = 
  let rec listcheck (str, namelist) = 
    match namelist with
    |[] -> false
    |a::lst -> 
      if(a=str) then true else listcheck(str, lst)
  in
  let rec metrolistcheck (met, arealist) =
    match met with
    |STATION a -> listcheck(a, arealist)
    |AREA(a, m) -> metrolistcheck(m, a::arealist)
    |CONNECT(m1, m2) -> metrolistcheck(m1, arealist) && metrolistcheck(m2, arealist)
  in
  metrolistcheck(met,[])



  
