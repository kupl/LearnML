type aexp =
    Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

exception InvalidArgument

let rec diff:aexp*string -> aexp=
    fun (a,str)->
	    match a with
		|Const n -> Const 0
		|Var s -> 
		    if s=str then Const 1
			else Const 0
		|Power (s,n) -> 
		    if s=str then
			    if n=2 then Times [Const 2;Var s]
				else if n=1 then Const 1
				else if n=0 then Const 0
				else Times [Const n;Power(s,(n-1))]
			else Const 0
		|Times [] -> raise InvalidArgument
		|Times aexps ->
	        let clean aexps=
			    if (List.mem (Const 0) aexps) then Const 0
				else
				    let filt=(List.filter (fun x -> (x<>(Const 1))) aexps) in
					if (List.length filt)=0 then Const 0
					else if (List.length filt)=1 then List.hd filt
					else Times filt 
			in
			let new_aexps=(List.filter (fun n -> (n<>Const 0))
					        (List.map (fun x ->
		   	                               (clean (List.map (fun y -> 
							                              if (x=y) then (diff (y,str))
						                                  else y) aexps))) 
						aexps)) in
							
			if (List.length new_aexps)=0 then Const 0
			else if (List.length new_aexps)=1 then List.hd new_aexps
			else Sum new_aexps
		|Sum [] -> raise InvalidArgument
		|Sum aexps ->
            let new_aexps= 
			    (List.filter (fun n->(n<>Const 0))
				    (List.map (fun x-> (diff (x,str))) aexps)) in
			if (List.length new_aexps)=0 then Const 0
			else if (List.length new_aexps)=1 then List.hd new_aexps
			else Sum new_aexps
			
