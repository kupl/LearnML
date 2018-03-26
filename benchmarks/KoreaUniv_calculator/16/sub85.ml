exception NotImplemented
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int = fun exp -> (* raise NotImplemented*)  (* TODO *)
    let rec get_value parameter =
      begin 
        match parameter with
      | X -> raise NotImplemented  
      | INT n -> n 
      | ADD (n, m) -> (get_value n) + (get_value m) 
      | SUB (n, m) -> (get_value n) - (get_value m) 
      | MUL (n, m) -> (get_value n) * (get_value m) 
      | DIV (n, m) -> (get_value n) / (get_value m) 
      | SIGMA (n, m, ex) ->  
                let rec getexint k =
                  begin
                    match k with
                   | X -> (fun para->para) 
                   | INT n -> (fun para->n) 
                   | ADD (m,n) -> (fun para-> ((getexint m) para) + ((getexint n) para) ) 
                   | SUB (m,n) -> (fun para-> ((getexint m) para) - ((getexint n) para) )  
                   | MUL (m,n) -> (fun para-> ((getexint m) para) * ((getexint n) para) ) 
                   | DIV (m,n) -> (fun para-> ((getexint m) para) / ((getexint n) para) ) 
                   | SIGMA (m,n,r) -> raise NotImplemented
                  end
                 in
                 let rec getScope v1 v2 =
                   if v1>v2 then raise NotImplemented
                   else if v1=v2 then ((getexint ex) v2) 
                   else ( ((getexint ex) v1) + (getScope (1+v1) v2))
                 in (getScope (get_value n) (get_value m))
      end
    in match exp with
    | X -> 0 
    | INT n -> n
    | ADD (n,m) -> (get_value (ADD (n,m))) 
    | SUB (n,m) -> (get_value (SUB (n,m))) 
    | MUL (n,m) -> (get_value (MUL (n,m))) 
    | DIV (n,m) -> (get_value (DIV (n,m))) 
    | SIGMA (n,m,eex) -> (get_value (SIGMA (n,m,eex)));;
