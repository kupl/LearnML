
(* exercise 4 not yet*)
type var = string
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda

let check largein =
        let rec lambdarec smallin l = 
                match smallin with
                | V(x) -> (List.mem x l)
                | P(x, y) -> (lambdarec y (x::l))
                | C(y, z) -> ((lambdarec y l) && (lambdarec z l))
         in
         lambdarec largein []
