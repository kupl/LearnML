
(*#7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
  let f1 (x,_) (a,_) = x::a
    in let f2 (_,y) (_,b) = y::b
      in match lst with
      |[] -> ([],[])
      |(x,y)::tl -> (f1 (x,y) (unzip tl)),(f2 (x,y) (unzip tl));;
