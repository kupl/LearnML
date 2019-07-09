let rec reverse l =
  match l with
    |[]->[]
    | a::z -> (reverse z) @ [a]
    
let lst2int : int list -> int
= fun lst ->
  let rec hw l =
    match l with
      |[]->0
      |a::z-> a+10*(hw z) in
      hw(reverse lst);;
      
      
lst2int [2;3;4;5];;      
    
  
