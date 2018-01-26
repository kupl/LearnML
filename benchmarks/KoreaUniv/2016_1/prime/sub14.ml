let rec prime n = if n=1 || n=2 then true
				  else god n 2;;

let rec god a b = if b=a-1 then true
				  else if a mod b=0 then false
				  else god a (b+1);;
