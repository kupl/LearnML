(* problem 7*)
let rec unzip : ('a*'b) list -> 'a list * 'b list 
=fun lst->
  match lst with 
  []->([],[])
  |(f,s)::t-> 
    let l1, l2=unzip t 
  in f::l1 , s::l2;;