  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun (lb,rb) -> let rec bal : mobile -> bool * int
  = fun (lb,rb) -> begin match (lb,rb) with 
                    (SimpleBranch(w,x),SimpleBranch(y,z)) -> if w*x=y*z then (true,x+z) else (false,0)
                  | (CompoundBranch(w,x),SimpleBranch(y,z)) ->begin 
                                                              match bal(x) with 
                                                                (true,ww) -> if w*ww=y*z then (true,ww+z) else (false,0)
                                                              | _ -> (false,0)
                                                              end
                  | (SimpleBranch(y,z),CompoundBranch(w,x)) -> begin
                                                                 match bal(x) with 
                                                                 (true,ww) -> if w*ww=y*z then (true,ww+z) else (false,0)
                                                                | _ -> (false,0) 
                                                                end
                  | (CompoundBranch(y,z),CompoundBranch(w,x)) -> begin 
                                                                  match bal(x),bal(z) with
                                                                  (true,ww),(true,zz) -> if y*zz=w*ww then (true,zz+ww) else (false,0)
                                                                | _ -> (false,0)   
                                                                end
end
in match bal(lb,rb) with 
  (true,x) -> true
| _ -> false
