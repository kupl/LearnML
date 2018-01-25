(*6*)
let rec drop : 'a list -> int -> 'a list = fun l n ->
if n>0 then (if List.length l > 0 then drop( List.tl l) (n-1) else []) else l;;  