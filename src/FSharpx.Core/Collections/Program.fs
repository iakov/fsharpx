module small.Main

open System
module S = Microsoft.FSharp.Collections.Seq
open FSharpx.OptimizedSeq.Aggressive
open FSharpx.OptimizedSeq

// This rough tests are used to proof that OptimizedSeq is useful: performance 
// boost is observed even for small collections (10 elements) and for FSharpList (aka list<'a>)   
// downcasting makes sence and results into performance improvement against IEnumerator.

let runTestSet inputGenerator = 
  let iter =
    let r = ref 0
    fun x -> r := !r + x   
  
  let testSet: list<string * (#seq<_> -> unit) * (#seq<_> -> unit)>  =
    [ 
 //      "Seq.exists", S.exists (fun _ -> false) >> ignore, OS.exists (fun _ -> false) >> ignore
 //      "Seq.exists2", (fun i -> S.exists2 (fun _ _ -> false) i i |> ignore), (fun i -> OS.exists2 (fun _ _ -> false) i i |> ignore)
 //      "Seq.fold", S.fold (+) 0 >> ignore, OS.fold (+) 0 >> ignore
 //      "Seq.forall", S.forall (fun _ -> true) >> ignore, OS.forall (fun _ -> true) >> ignore
 //      "Seq.forall2",  (fun i -> S.forall2 (fun _ _ -> true) i i |> ignore), (fun i -> OS.forall2 (fun _ _ -> true) i i |> ignore)
 //      "Seq.iter", S.iter iter, OS.iter iter 
 //      "Seq.iter2", (fun i -> S.iter2 (fun a b -> iter (a + b)) i i), (fun i -> OS.iter2 (fun a b -> iter (a + b)) i i) 
 //      "Seq.nth", (fun (i:#seq<_>) -> S.nth (S.length i - 1) i |>ignore), (fun (i:#seq<_>) -> OS.nth (Seq.length i - 1) i |>ignore) 
 //      "Seq.tryPick", S.tryPick (fun _ -> None) >> ignore, OS.tryPick (fun _-> None) >> ignore  
       "Seq.map", S.map Some >> S.length >> ignore, Seq.map Some >> Seq.length >> ignore
    ]
                                 
  let measureFor size = 
    let prepare = 
      let input = inputGenerator size
      fun test () -> try test input with e -> ()
      
    let numberOfIterations = 20000000/size
    
    let compareTwoRuntimes count name test1 test2 =
      let avg1 = prepare test1 |> FSharpx.TimeMeasurement.stopAverageTime count |> snd 
      let avg2 = prepare test2 |> FSharpx.TimeMeasurement.stopAverageTime count |> snd
      printfn "%-20s\t%.2g" name <| (avg1 / avg2)
      float count * avg1, float count * avg2
               
    printfn "\n%d elements, %d iterations" size numberOfIterations
    testSet 
    |> List.map (fun x -> x |||> compareTwoRuntimes numberOfIterations)
    |> List.fold (fun (r1,r2) (e1,e2) -> (r1 + e1, r2 + e2)) (0., 0.)
    |> fun res -> res ||> printfn "Total Seq vs OptimizedSeq: %.0fms vs %.0fms"   
  List.iter measureFor <| [10; 10000; 10000000]
  
  
let forArray () =
  printfn "\nArray"  
  runTestSet <| fun length -> Array.init length id 
  
let forResizeArray () = 
  printfn "\nResizeArray (aka System.Collections.Generic.List<T>)"  
  runTestSet <| fun length -> Microsoft.FSharp.Collections.ResizeArray.init length id 
  
let forList () =
  printfn "\nList (aka list<'a> aka FSharpList<T>)" 
  runTestSet <| fun length -> List.init length id 
        
printfn "Seq vs OptimizedSeq performance ratio (Seq/OptimizedSeq stop time).\nGreater ratio is better (for OptimizedSeq). Must be at least > 1.2"                        
forList()
forResizeArray()
forArray()