module Anat.Tests

open Anat
open Swensen.Unquote
open Xunit

(* Fixtures

   Simple sample functions to test the various Arrow operations, for both value
   functions and async value functions. *)

(* Value Functions *)

let f1 =
    function | true -> 1
             | _ -> 0

let f2 =
    function | 1 -> "true"
             | _ -> "false"

let f3 =
    function | true -> "true"
             | _ -> "false"

(* Async Value Functions *)

let asyncf1 =
    function | true -> async.Return 1
             | _ -> async.Return 0

let asyncf2 =
    function | 1 -> async.Return "true"
             | _ -> async.Return "false"

let asyncf3 =
    function | true -> async.Return "true"
             | _ -> async.Return "false"

(* Tests

   Tests for the various operations supported by the Arrow implementation for
   both value functions and async value functions. *)

(* Arrow.arrow *)

[<Fact>]
let ``Arrow.arrow creates an Arrow`` () =
    Arrow.run (Arrow.arrow f1) true =! 1
    Async.RunSynchronously (Arrow.run (Arrow.arrow asyncf1) true) =! 1

(* Arrow.compose *)

[<Fact>]
let ``Arrow.compose composes two Arrows`` () =
    Arrow.run (Arrow.compose f1 f2) true =! "true"
    Async.RunSynchronously (Arrow.run (Arrow.compose asyncf1 asyncf2) true) =! "true"

(* Arrow.first *)

[<Fact>]
let ``Arrow.first applies an Arrow to the first of a pair of inputs`` () =
    Arrow.run (Arrow.first f1) (true, true) =! (1, true)
    Async.RunSynchronously (Arrow.run (Arrow.first asyncf1) (true, true)) =! (1, true)

(* Arrow.second *)

[<Fact>]
let ``Arrow.second applies an Arrow to the second of a pair of inputs`` () =
    Arrow.run (Arrow.second f1) (true, true) =! (true, 1)
    Async.RunSynchronously (Arrow.run (Arrow.second asyncf1) (true, true)) =! (true, 1)

(* Arrow.fanout *)

[<Fact>]
let ``Arrow.fanout applies two Arrows to a single input`` () =
    Arrow.run (Arrow.fanout f1 f3) true =! (1, "true")
    Async.RunSynchronously (Arrow.run (Arrow.fanout asyncf1 asyncf3) true) =! (1, "true")

(* Arrow.split *)

[<Fact>]
let ``Arrow.split applies two Arrows to a pair of inputs`` () =
    Arrow.run (Arrow.split f1 f2) (true, 0) =! (1, "false")
    Async.RunSynchronously (Arrow.run (Arrow.split asyncf1 asyncf2) (true, 0)) =! (1, "false")