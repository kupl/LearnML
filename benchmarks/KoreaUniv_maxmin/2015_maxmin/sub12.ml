let rec max : int list -> int
=fun l ->  
match l with
| [] -> 0
| hd::tl -> (match tl with
      |[] -> hd +0
      |hd1::tl1 -> if hd>hd1 then max(hd::tl1) else max(hd1::tl1)
    );; 

let rec min : int list -> int
=fun l -> 
match l with
| [] -> 0
| hd::tl -> (match tl with
      |[] -> hd +0
      |hd1::tl1 -> if hd<hd1 then min(hd::tl1) else min(hd1::tl1)
    );; 
