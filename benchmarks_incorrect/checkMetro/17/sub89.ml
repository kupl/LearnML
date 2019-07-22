type metro = STATION of name
             | AREA of name * metro
             | CONNECT of metro * metro
and name = string

let rec matchid ((x:string),(l:string list)) :bool =
	match l with
	[]-> false
	|hd::tl-> if (hd=x) then true else matchid(x,tl)

let rec ckMetro ((m:metro),(idl:string list)) :bool = 
	match m with 
		STATION x-> matchid(x,idl) 
		|AREA (id,STATION x)-> ckMetro(STATION x,id::idl)  
		|AREA (id,some)-> ckMetro(some,id::idl) 
		|CONNECT (STATION x,STATION y)-> matchid(x,idl)&&matchid(y,idl)
		|CONNECT (STATION x,some)-> if (matchid(x,idl)) then ckMetro(some,idl) else false
		|CONNECT (x,y)-> ckMetro(x,idl)&&ckMetro(y,[])

let rec checkMetro (m:metro) :bool = ckMetro(m,[])




