  type exp = V of var
           | P of var * exp 
           | C of exp * exp
  and var = string
  
  let check ex=
    let rec excheck y x_list =
      match m with
        | V n -> List.mem n n_list
        | P (x,y1) -> excheck m1 (x::x_list)
        | C (x1, y2) -> excheck m1 x_list && excheck y2 x_list
  in
  excheck ex []

