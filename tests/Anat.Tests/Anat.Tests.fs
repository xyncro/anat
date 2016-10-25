module Anat.Tests

open System
open Anat
open Xunit
#if NET451
open Swensen.Unquote
#else
let inline (=!) x y = Assert.StrictEqual(x, y)
#endif

(* Fixtures

   Simple sample functions to test the various Arrow operations, for both
   simple functions (_ -> _) and Func types (Func<_,_>). *)

(* _ -> _ *)

let f1 =
    function | true -> 1
             | _ -> 0

let f2 =
    function | 1 -> "true"
             | _ -> "false"

let f3 =
    function | true -> "true"
             | _ -> "false"

(* Func<_,_> *)

let ff1 =
    Func<_,_> (function | true -> 1
                        | _ -> 0)

let ff2 =
    Func<_,_> (function | 1 -> "true"
                        | _ -> "false")

let ff3 =
    Func<_,_> (function | true -> "true"
                        | _ -> "false")

(* Tests

   Tests for the various operations supported by the Arrow implementation for
   both value functions and async value functions. *)

(* Arrow.arrow *)

[<Fact>]
let ``Arrow.arrow creates an Arrow`` () =
    Arrow.arrow f1 true =! 1
    (Arrow.arrow ff1).Invoke true =! 1

(* Arrow.compose *)

[<Fact>]
let ``Arrow.compose composes two Arrows`` () =
    Arrow.compose f1 f2 true =! "true"
    (Arrow.compose ff1 ff2).Invoke true =! "true"

(* Arrow.first *)

[<Fact>]
let ``Arrow.first applies an Arrow to the first of a pair of inputs`` () =
    Arrow.first f1 (true, true) =! (1, true)
    (Arrow.first ff1).Invoke (true, true) =! (1, true)

(* Arrow.second *)

[<Fact>]
let ``Arrow.second applies an Arrow to the second of a pair of inputs`` () =
    Arrow.second f1 (true, true) =! (true, 1)
    (Arrow.second ff1).Invoke (true, true) =! (true, 1)

(* Arrow.fanout *)

[<Fact>]
let ``Arrow.fanout applies two Arrows to a single input`` () =
    Arrow.fanout f1 f3 true =! (1, "true")
    (Arrow.fanout ff1 ff3).Invoke true =! (1, "true")

(* Arrow.split *)

[<Fact>]
let ``Arrow.split applies two Arrows to a pair of inputs`` () =
    Arrow.split f1 f2 (true, 0) =! (1, "false")
    (Arrow.split ff1 ff2).Invoke (true, 0) =! (1, "false")
