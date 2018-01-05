module type Queue =
   sig
     type element
     type queue
     exception EMPTY_Q
     val emptyQ: queue
     val enQ: queue * element -> queue
     val deQ: queue -> element * queue
   end
   module IntListQ : Queue =
     struct
       type element = int list
       type queue = {
         lq : int list;
         rq : int list;}
       exception EMPTY_Q
       let emptyQ = {lq =[]; rq=[]}
       let enQ (q,(e:int list)) =(
         if (List.length q.rq)= 0 then
           {lq=[];
           rq=List.rev (List.append e q.lq)}
         else
           {lq=List.append e q.lq;
           rq=q.rq}
       )
       let deQ q =(
         if (List.length q.rq)= 0 then
           raise EMPTY_Q
         else (
           if (List.length q.rq)=1 then(
           (
               [(List.hd q.rq)],
               {lq =[];
                rq = List.rev q.lq
               })
           )
           else 
             ([(List.hd q.rq)],{lq = q.lq;
             rq = List.tl q.rq})
         )
       )
     end
