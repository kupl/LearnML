(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec simplifier f = let res = begin match f with
  |Times l->if List.exists (fun x->if x=Const 0 then true else false) l then Const 0
						else begin match l with
							|[]->Times []
							|hd::tl->let lst = begin match simplifier (Times tl) with
												|Times lst->lst
												|_->[] end in Times ((simplifier hd)::lst) end
  |Sum l->begin match l with
    |[]->Sum []
    |hd::tl->if hd=Const 0 then Sum tl
						 else let lst = begin match simplifier (Sum tl) with
                   |Sum lst->lst
                   |_->[] end in Sum ((simplifier hd)::lst) end
  |Power (s,n)->begin match n with
		|0->Const 1
		|1->Var s
		|_->f end
  |_->f end in if f=res then res else simplifier res


let rec diff : aexp * string -> aexp
= fun (e,x) -> let res = begin match e with
  |Const n->Const 0
  |Var s->if s=x then Const 1 else Const 0
  |Times l->begin match l with
    |[]->Const 0
    |hd::tl->Sum [Times ((diff (hd,x))::tl); Times [hd; (diff (Times tl, x))]] end
  |Sum l->begin match l with
		|[]->Const 0
    |[y]->diff (y,x)
    |hd::tl->let l1=[diff (hd,x)] in 
      let l2 = begin match diff (Sum tl,x) with
       |Sum l->l1@l
       |i->l1@[i] end
      in Sum l2 end
  |Power (s,n)->begin match (s,n) with
    |(_,0)->Const 0
    |(s,1)->diff (Var s,x)
    |(p,q)->if p=x then Times [Const q;Power (p, q-1)] else Const 0 end
  end in simplifier res