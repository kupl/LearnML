let rec filter pred lst =  (* (’a -> bool) -> ’a list -> ’a list  *)
  match lst with
    |[] -> []
    |h::t -> 
        match (pred h) with
          |true ->h::(filter pred t)
          |false -> (filter pred t)
