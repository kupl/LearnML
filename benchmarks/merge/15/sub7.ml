let rec merge ((a:int list),(b:int list)) =
    match (a,b) with
    | ([],[]) -> [] 
    | ([x],[]) -> [x]
    | ([],[y]) -> [y]
    | (hda::tla,[]) -> 
        hda::merge(tla,[])
    | ([],hdb::tlb) -> 
        hdb::merge([],tlb)
    | (hda::tla, hdb::tlb) -> (
        if(hda > hdb) then (
            hda::merge(tla,b)       
        ) else (
            hdb:: merge(a,tlb)
        ) ) 
(*
 
in
    let t1 = [64;8;5;2];
    and t2 = [63;10;9;7;4];
in     
    let rec print_list li =
        match li with
    | [] -> ()
    | e::l -> print_int e ; print_string " " ; print_list l
 in 
    print_list(merge(t1,t2) ) 

*)




