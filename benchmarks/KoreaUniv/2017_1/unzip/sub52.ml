(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
 match lst with
    |[] -> ([], [])
    |(head1,head2)::tail -> 
    let (headoftail1, headoftail2) = unzip tail in 
    (head1::headoftail1, head2::headoftail2)