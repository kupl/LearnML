type metro=STATION of name
|AREA of name * metro
|CONNECT of metro* metro
and
name=string



let rec subcheckMetro m l=
match m with
|STATION a -> List.mem a l
|AREA (a, b) ->subcheckMetro b (a::l)
|CONNECT (a, b)->subcheckMetro a l && subcheckMetro b l

let checkMetro m=
subcheckMetro m []
