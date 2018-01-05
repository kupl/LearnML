open List
open String

type metro=
    STATION of name
	|AREA of name*metro
	|CONNECT of metro*metro
and name=string

let rec remainStation: metro->name list=
    fun me->
	    match me with
	    |STATION n -> [n]
		|AREA (n,m) -> List.filter (fun x -> (String.compare n x)!=0) (remainStation m)
	    |CONNECT (lm,rm) ->List.append (remainStation lm) (remainStation rm)

let checkMetro: metro->bool=
    fun me->
	    (List.length (remainStation me))=0
		    
