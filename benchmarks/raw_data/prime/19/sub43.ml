
let prime : int -> bool
= fun n ->
 let rec loop i=
   match n mod i with
     |0-> if i=n then true else false
     |_-> loop (i+1) in loop 2;;
