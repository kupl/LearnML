type var = string

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

let check : lambda -> bool = fun e -> true
