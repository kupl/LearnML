let rec zipper : int list * int list -> int list
  = fun (a,b) -> 
    match a with
        [] -> b
      | h::t -> h::
                  (match b with
                      [] -> t
                    | h2::t2 -> h2::(zipper (t,t2)));;
