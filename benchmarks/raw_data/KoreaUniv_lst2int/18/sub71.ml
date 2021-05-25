
let lst2int : int list -> int
= fun lst ->
 
  let rec listint lst n=match lst with
    |[]->n
    |hd::tl ->listint tl ((n*10)+hd) in listint lst 0;;
  
    

lst2int [2;3;4;5];;