let rec merge : int list * int list -> int list = fun (a , b) ->
  if a == [] && b == [] then []
  else if a == [] && b != [] then List.hd b :: merge (a , List.tl b)  
  else if a != [] && b == [] then List. hd a :: merge (List.tl a, b)
  else if List.hd a <= List.hd b then List.hd b :: merge (a , List.tl b)
  else List.hd a :: merge (List.tl a , b)