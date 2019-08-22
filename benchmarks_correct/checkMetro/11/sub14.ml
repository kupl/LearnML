type lambda = V of var
        | P of var * lambda
	    | C of lambda * lambda
and var = string

let check met =

let rec checklist met l =
    match met with
	    V s -> List.mem s l
	    | P (n, m) -> (checklist m (n::l))
	    | C (m1, m2) -> (checklist m1 l) && (checklist m2 l)
in
(checklist met [])


