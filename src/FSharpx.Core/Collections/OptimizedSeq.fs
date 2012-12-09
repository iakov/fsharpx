/// This module aims to provide zero performance drawback 
/// when standard collections are used via IEnumerable interface, i.e. trough Seq module functions.
/// To achieve the goal standard Seq module functions are re-implemented in a <i>safe</i> manner
/// and each new corresponding realization attemts to provide the best performance
/// result based on knowledge of the underlying collection structure.
/// To substitute qualified calls to standard Seq module functions by optimized versions 
/// just <c>open FSharpx.OptimizedSeq</c>. To substitute unqualified calls use <c>open FSharpx.OptimizedSeq.Seq</c>
/// To get more boost try also <c>open FSharpx.OptimizedSeq.Aggressive[.Seq]</c>, but read the note for that module carefully.

namespace FSharpx.OptimizedSeq
module S = Microsoft.FSharp.Collections.Seq
module RA = Microsoft.FSharp.Collections.ResizeArray

/// Contains additions to other modules
module private Helpers =
  /// Additional methods for ResizeArray
  module ResizeArray =
    type private ra<'a> = ResizeArray<'a>
    let pick chooser ra = 
      let result = RA.tryPick chooser ra
      match result with
        | None -> S.pick chooser S.empty // to simulate original exception 
        | Some result -> result   

(* TBD: Seems like no boost observed for following functions even for array. Performance tests must be redone.
average
averageBy 
max
maxBy
min
minBy
sum
sumBy
*)  
module Seq =           
  let exists pred (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.exists pred a
      | :? ResizeArray<_> as ra -> RA.exists pred ra
      | :? list<_> as l -> List.exists pred l
      | _ -> S.exists pred s  

  let exists2 predicate (s1:seq<_>) (s2:seq<_>) = 
    match s1, s2 with
      | :? array<_> as a1, (:? array<_> as a2) -> Array.exists2 predicate a1 a2
      | :? ResizeArray<_> as ra1, (:? ResizeArray<_> as ra2) -> RA.exists2 predicate ra1 ra2
      | :? list<_> as l1, (:? list<_> as l2) -> List.exists2 predicate l1 l2
      | _ -> S.exists2 predicate s1 s2

  let find pred (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.find pred a
      | :? ResizeArray<_> as ra -> RA.find pred ra
      | :? list<_> as l -> List.find pred l
      | _ -> S.find pred s  

  let findIndex pred (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.findIndex pred a
      | :? ResizeArray<_> as ra -> RA.findIndex pred ra
      | :? list<_> as l -> List.findIndex pred l
      | _ -> S.findIndex pred s  

  let fold f init (s:seq<_>) =
    match s with
      | :? array<_> as a -> Array.fold f init a
      | :? ResizeArray<_> as ra -> RA.fold f init ra
      | :? list<_> as l -> List.fold f init l
      | _ -> S.fold f init s 

  let forall pred (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.forall pred a
      | :? ResizeArray<_> as ra -> RA.forall pred ra
      | :? list<_> as l -> List.forall pred l
      | _ -> S.forall pred s  

  let forall2 predicate (s1:seq<_>) (s2:seq<_>) = 
    match s1, s2 with
      | :? array<_> as a1, (:? array<_> as a2) -> Array.forall2 predicate a1 a2
      | :? ResizeArray<_> as ra1, (:? ResizeArray<_> as ra2) -> RA.forall2 predicate ra1 ra2
      | :? list<_> as l1, (:? list<_> as l2) -> List.forall2 predicate l1 l2
      | _ -> S.forall2 predicate s1 s2

  let iter f (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.iter f a
      | :? ResizeArray<_> as ra -> RA.iter f ra
      | :? list<_> as l -> List.iter f l
      | _ -> S.iter f s

  let iter2 f (s1:seq<_>) (s2:seq<_>) = 
    match s1, s2 with
      | :? array<_> as a1, (:? array<_> as a2) -> Array.iter2 f a1 a2
      | :? ResizeArray<_> as ra1, (:? ResizeArray<_> as ra2) -> RA.iter2 f ra1 ra2
      | :? list<_> as l1, (:? list<_> as l2) -> List.iter2 f l1 l2
      | _ -> S.iter2 f s1 s2
   

  let iteri f (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.iteri f a
      | :? ResizeArray<_> as ra -> RA.iteri f ra
      | :? list<_> as l -> List.iteri f l
      | _ -> S.iteri f s

  let nth index (s:seq<_>) = 
    match s with
      | :? array<_> as a -> a.[index]
      | :? ResizeArray<_> as ra ->  ra.[index]
      | :? list<_> as l -> List.nth l index
      | _ -> S.nth index s
              
  let pick chooser (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.pick chooser a
      | :? ResizeArray<_> as ra -> Helpers.ResizeArray.pick chooser ra   
      | :? list<_> as l -> List.pick chooser l
      | _ -> S.pick chooser s  
                
  let reduce f (s:seq<_>) =
    match s with
      | :? array<_> as a -> Array.reduce f a
      | :? ResizeArray<_> as ra -> RA.reduce f ra
      | :? list<_> as l -> List.reduce f l
      | _ -> S.reduce f s 

  let tryFind pred (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.tryFind pred a
      | :? ResizeArray<_> as ra -> RA.tryFind pred ra
      | :? list<_> as l -> List.tryFind pred l
      | _ -> S.tryFind pred s  

  let tryFindIndex predicate (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.tryFindIndex predicate a
      | :? ResizeArray<_> as ra -> RA.tryFindIndex predicate ra
      | :? list<_> as l -> List.tryFindIndex predicate l
      | _ -> S.tryFindIndex predicate s  

  let tryPick chooser (s:seq<_>) = 
    match s with
      | :? array<_> as a -> Array.tryPick chooser a
      | :? ResizeArray<_> as ra -> RA.tryPick chooser ra
      | :? list<_> as l -> List.tryPick chooser l
      | _ -> S.tryPick chooser s  


module Aggressive =
  /// This module contains additional re-implemented Seq functions. 
  /// Use this module to get extra boost when underlying datastructure is standard one 
  ///  and laziness does not matter. These functions are `aggressive' in both 
  ///  meanings: extra boost and non-lazy sequence evaluation. 
  module Seq = 
    let choose chooser (s:seq<_>) : seq<_>  = 
      match s with
        | :? array<_> as a -> upcast Array.choose chooser a 
        | :? ResizeArray<_> as ra -> upcast RA.choose chooser ra
        | :? list<_> as l -> upcast List.choose chooser l
        | _ -> S.choose chooser s  

    let filter predicate (s:seq<_>) : seq<_>  = 
      match s with
        | :? array<_> as a -> upcast Array.filter predicate a 
        | :? ResizeArray<_> as ra -> upcast RA.filter predicate ra
        | :? list<_> as l -> upcast List.filter predicate l
        | _ -> S.filter predicate s
        
    let map mapping (s:seq<_>) : seq<_> =
      match s with
        | :? array<_> as a -> upcast Array.map mapping a 
        | :? ResizeArray<_> as ra -> upcast RA.map mapping ra
        | :? list<_> as l -> upcast List.map mapping l
        | _ -> S.map mapping s
          
    let mapi mapping (s:seq<_>): seq<_> = 
      match s with
        | :? array<_> as a -> upcast Array.mapi mapping a 
        | :? ResizeArray<_> as ra -> upcast RA.mapi mapping ra
        | :? list<_> as l -> upcast List.mapi mapping l
        | _ -> S.mapi mapping s
       
    let map2 mapping (s1:seq<_>) (s2:seq<_>) : seq<_> = 
      match s1, s2 with
        | :? array<_> as a1, (:? array<_> as a2) -> upcast Array.map2 mapping a1 a2
        | :? ResizeArray<_> as ra1, (:? ResizeArray<_> as ra2) -> upcast RA.map2 mapping ra1 ra2
        | :? list<_> as l1, (:? list<_> as l2) -> upcast List.map2 mapping l1 l2
        | _ -> S.map2 mapping s1 s2
    
