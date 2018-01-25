(*problem 7*)
  let rec unzip lst = match lst with
                    | []->([],[])
                    | (x,y)::tl -> 
                          (match unzip tl with (a,b) -> (x::a,y::b));;
