let rec max l=match l with
        |[]->0
  	|a::[]->a
  	|a::b->
  		let c=max b in
  		if a>c then a
  		else c

let rec min l=match l with
        |[]->0
  	|a::[]->a
  	|a::b->
  		let c=min b in
  		if a<c then a
  		else c
