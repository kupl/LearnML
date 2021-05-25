(*Cedric Brown*)
let rec search s = function
| [] -> false
| head::tail -> head = s || search s tail

let rec app : 'a list -> 'a list -> 'a list
= fun list1 list2 ->
  match list1 with
    | [] -> list2
    | head::tail -> 
      if search head list2 then app tail list2 
      else app tail (head::list2);;
(*Example from hw pdf!*)      
app [4;5;6;7] [1;2;3;4];;