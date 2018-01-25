(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec mkl1 list = match list with
|[] -> []
|(x,y)::tl -> x:: (mkl1 tl)
  in let rec mkl2 list = match list with
  |[] ->[]
  |(x,y)::tl -> y:: (mkl2 tl)
   in ((mkl1 lst),(mkl2 lst))