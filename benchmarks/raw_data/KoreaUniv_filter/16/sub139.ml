let rec filter pred lst = 
let rec fold f l =
match l with
|[]->[]
|hd::tl-> if(f hd) then hd::(fold f tl) else fold f tl  in
fold pred lst;;
