let rec merge (a, b) = 
  match a with
  |[] -> b
  |head_a::tail_a ->
      match b with
      |[] -> a
      |head_b::tail_b ->
	  (if (head_a - head_b > 0) 
	  then head_a::merge(tail_a, b)
	  else head_b::merge(a, tail_b))
	    
