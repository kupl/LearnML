let unzip : ('a * 'b) list -> 'a list * 'b list
  = fun lst -> let rec u lst = match lst with | [] -> [] | (y,v)::tl -> y::u tl
    in let rec z lst = match lst with | [] -> [] | (y,v)::tl -> v::z tl
      in let uz lst = match lst with | [] -> [],[] | (y,v)::tl -> u lst,z lst in uz lst
