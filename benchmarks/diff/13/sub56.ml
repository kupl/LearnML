open String
open List
open Array

type ae =
    CONST of int
	|VAR of string
	|POWER of string * int
	|TIMES of ae list
	|SUM of ae list

exception InvalidArgument

let rec diff:ae*string -> ae=
    fun (a,str)->
	    match a with
		|CONST n -> CONST 0;
		|VAR s -> 
		    if (String.compare s str)=0 then CONST 1
			else CONST 0
		|POWER (s,n) -> 
		    if (String.compare s str)=0 then
			    if n=2 then TIMES [CONST 2;VAR s]
				else if n=1 then CONST 1
				else if n=0 then CONST 0
				else TIMES [CONST n;POWER(s,(n-1))]
			else CONST 0
		|TIMES [] -> raise InvalidArgument
		|TIMES aes ->
	        let clean aes=
			    if (List.mem (CONST 0) aes) then CONST 0
				else
				    let filt=(List.filter (fun x -> (x<>(CONST 1))) aes) in
					if (List.length filt)=0 then CONST 0
					else if (List.length filt)=1 then List.hd filt
					else TIMES filt 
			in
			let new_aes=(List.filter (fun n -> (n<>CONST 0))
					        (List.map (fun x ->
		   	                               (clean (List.map (fun y -> 
							                              if (x=y) then (diff (y,str))
						                                  else y) aes))) 
						aes)) in
							
			if (List.length new_aes)=0 then CONST 0
			else if (List.length new_aes)=1 then List.hd new_aes
			else SUM new_aes
		|SUM [] -> raise InvalidArgument
		|SUM aes ->
            let new_aes= 
			    (List.filter (fun n->(n<>CONST 0))
				    (List.map (fun x-> (diff (x,str))) aes)) in
			if (List.length new_aes)=0 then CONST 0
			else if (List.length new_aes)=1 then List.hd new_aes
			else SUM new_aes
			
