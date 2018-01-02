type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

let rec  check : exp -> bool
	= fun exp -> 
  match exp with 
|P ("a", e) ->(match e with 
               |V "b" -> if "b"= "a" then true else false
               |C(V "b", V"c")-> if "b"="a"&&"c"="a" then true else false
               |P("b", V "c") -> if "c"="b"||"c"="a" then true else false) 
|_ -> false 