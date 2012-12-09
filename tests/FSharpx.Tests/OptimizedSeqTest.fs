module FSharpx.Tests.OptimizedSeqTests

open System
open Microsoft.FSharp.Collections
open FSharpx.Collections
open NUnit.Framework
open FsUnit

let size = 10
let source = { 0 .. size } 
let array = Array.ofSeq source
let resizeArray = ResizeArray.ofSeq source
let list = List.ofSeq source

let collections = [source; upcast array; upcast resizeArray; upcast list]

let test f = 
    let results = List.map f collections
    results.Tail |> List.iter (shouldEqual results.Head)
    

let testPredicate x = x >= size - 1

[<Test>]
let exists() = test <| Seq.exists testPredicate
[<Test>]
let find () = test <| Seq.find testPredicate
[<Test>]
let findIndex () = test <| Seq.findIndex testPredicate
[<Test>]
let fold () = test <| Seq.fold (+) 0
[<Test>]
let forall () = test <| Seq.forall (not << testPredicate)
(*[<Test>]
let iter f () =  
[<Test>]
let iteri f () = 
*)
[<Test>]
let nth () = test <| Seq.nth size
(*[<Test>]
let pick chooser (s:seq<_>) = 
[<Test>]
let reduce f (s:seq<_>) = 
[<Test>]
let tryFind pred (s:seq<_>) = 
[<Test>]
let tryFindIndex predicate (s:seq<_>) = 
[<Test>]
let tryPick chooser (s:seq<_>) = 
 
*)
(*
  let exists2 predicate (s1:seq<_>) (s2:seq<_>) = 
  let forall2 predicate (s1:seq<_>) (s2:seq<_>) = 
  let iter2 f (s1:seq<_>) (s2:seq<_>) = 
  *)

