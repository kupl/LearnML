type lambda = V of var
|P of var * lambda
|C of lambda * lambda
and var = string

let check lambda =
let rec real_cM lambda lst =
match lambda, lst with
|V s, l -> (List.mem s l)
|P (var, met), l -> (real_cM met (l@[var]))
|C (met1, met2), l -> (real_cM met1 l) && (real_cM met2 l)
in real_cM lambda []
