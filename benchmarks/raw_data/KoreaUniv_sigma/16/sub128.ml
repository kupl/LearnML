let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->0 (* if a > b then 0 else f(a) + sigma(f,a+1,b) Could not get ocaml to accept this without 'a->'b->int not being in error*);;					
