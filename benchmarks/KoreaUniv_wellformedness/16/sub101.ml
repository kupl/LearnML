
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp ->
  match exp with 
  |P(s, V a)                -> if (s= a) then true else false
  |P(s, P(s1, e1))          -> 
    begin match e1 with 
        |C(e2, e3)->   ( ( check(P(s, e2)) ||check (P(s1, e2)) )&&( check (P(s, e3))||check (P(s1, e3)) ) )
        |_        ->   if ((check (P(s1,e1)) =true)||(check (P(s,e1)))) then true else false
      end
  |P(s, C(e1, e2))          ->
    begin match (e1, e2) with 
      |(P(s1, e3), e)    -> if (check (P(s, P(s1,e3))) &&check(P(s, P(s1,e)))) then true else false
      |(e, P(s1, e3))    -> if (check (P(s, P(s1,e3))) &&check(P(s, P(s1,e)))) then true else false
      |_                 -> if (check (P(s, e1))&& check(P(s,e2)))             then true else false
      end 
  |_ -> false 
