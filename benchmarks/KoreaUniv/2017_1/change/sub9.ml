(* problem 8*)

let change : int list -> int -> int
= fun coins amount ->
if amount<0 then 0 else
match (coins,amount) with
| ([], _) -> 0
| (_, 0) -> 1
| (hd::tl,_) ->
let cnt = Array.make (amount+1) 0 in cnt.(0) <-1;
List.iter (fun coin -> for i = coin to amount do cnt.(i) <- cnt.(i)+cnt.(i-coin) done) coins;
cnt.(amount) 
