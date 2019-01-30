type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string



let rec check : lambda -> bool
= fun lam -> match lam with
  | V v-> false
  | P(v,l)->begin match l with
    | V v'->if v'=v then true else false
    | P(v',l')->begin match l' with
      | V v''->if(v=v'')||(v'=v'') then true else false
      
      | C(l1,l2)->check( P(v,P(v',l1)))&&check (P(v,P(v',l2)))
      |_->assert false
      end
    | C(l',l'')-> check (P(v,l'))&&check (P(v,l''))
    end
  | C(l,l')->match l,l' with
    | V v,_->false
    | _,V v-> false
    | _-> check l && check l' ;;
    
    
