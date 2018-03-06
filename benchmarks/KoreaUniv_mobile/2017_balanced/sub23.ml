(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let fst p = match p with (x,_) -> x
let snd p = match p with (_,x) -> x

let rec balandw
= fun m ->
    let (br1,br2) = m in
    match br1 with
      SimpleBranch (l1,w1) ->(
        match br2 with
          SimpleBranch (l2,w2) ->
            if (l1*w1) = (l2*w2) then (true,(w1+w2))
            else (false, 0)
        | CompoundBranch (l2,sub2) -> 
            let res2  = balandw sub2 in
            if (fst res2) && ( (l1*w1) = (l2*(snd res2)) ) then (true,(w1+(snd res2)))
            else (false, 0)
      )
    | CompoundBranch (l1,sub1) ->(
        match br2 with
          SimpleBranch (l2,w2) ->
            let res1 = balandw sub1 in
            if (fst res1) && ( (l2*w2) = (l1*(snd res1)) ) then (true,(w2+(snd res1)))
            else (false, 0)

        | CompoundBranch (l2,sub2) ->
            let res1 = balandw sub1 in
            let res2 = balandw sub2 in
            if (fst res1) && (fst res2) && ( (l1*(snd res1)) = (l2*(snd res2)) ) then (true,((snd res1)+(snd res2)))
            else (false, 0)
      )

let balanced : mobile -> bool
= fun m ->
    fst (balandw m)
