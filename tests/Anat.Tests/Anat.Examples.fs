module Anat.Examples

open Anat
open Anat.Operators
open Swensen.Unquote
open Xunit

(* Examples

   Examples (with tested outputs to give some extra proof against regressions)
   of functions built using Arrows. Basic functions like a simple conceptual
   branching adder show simple usage of Arrows with the built in support for
   arrow inference of basic types 'a -> 'b (where 'b must support equality).

   More complex examples such as the usage of circuits show the ability to
   make custom types instances of Arrow and use them appropriately. *)

(* Adder

   The simple stateless adding Arrow given as part of the examples at
   https://www.haskell.org/arrows/syntax.html

   We are forced to give some type annotations to help the type inference
   along, helping to infer the type of >>> in this case. We could equally
   specify the types of f or g with the same result. *)

let addA f g : int -> int =
    f &&& g >>> (fun (y, z) -> y + z)

[<Fact>]
let ``addA behaves correctly`` () =
    Arrow.run (addA ((*) 2) ((/) 3)) 3 =! 7

(* Circuits

   Circuits shows a more advanced application of Arrows, defining a new type
   and the implementation of it to inhabit Arrow. This circuit example is
   taken from the Haskell Wiki Arrow Tutorial
   (https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial) although only the
   implentation down to mean is shown here - it's a good introduction to arrows
   and circuits without bringing in too much baggage. *)

(* Circuit *)

type Circuit<'a,'b> =
    | Circuit of ('a -> Circuit<'a,'b> * 'b)

(* Circuit Functions *)

[<RequireQualifiedAccess>]
module Circuit =

    let rec create f =
        Circuit (fun a -> create f, f a)

    let rec compose (Circuit f) (Circuit g) =
        Circuit (fun a ->
            let f', b = f a
            let g', c = g b

            compose f' g', c)

    let rec fanout (Circuit f) (Circuit g) =
        Circuit (fun a ->
            let f', b = f a
            let g', c = g a

            fanout f' g', (b, c))

    let rec run (Circuit f) =
        function | x :: xs -> f x |> fun (f', x') -> x' :: run f' xs
                 | _ -> []

(* Circuit Arrow *)

type Circuit<'a,'b> with

    static member Arrow (f: Circuit<'a,'b>) =
        f

    static member Compose f =
        fun g -> Circuit.compose f g

    static member Fanout f =
        fun g -> Circuit.fanout f g

(* Circuit Helpers *)

let circuit =
    Circuit.create

let uncurry f =
    fun (a, b) -> f a b

(* Circuit Primitives *)

let rec accum acc f =
    Circuit (fun a -> f a acc |> fun (a', acc') -> accum acc' f, a')

let accum' acc f =
    accum acc (fun a acc -> f a acc |> fun acc' -> (acc', acc'))

let total =
    accum' 0. (+)

let constant x =
    Circuit.create (fun _ -> x)

(* Circuit Primitive Tests *)

[<Fact>]
let ``total behaves correctly`` () =
    Circuit.run total [ 1.; 0.; 1.; 0.; 0.; 2. ] =! [ 1.; 1.; 2.; 2.; 2.; 4. ]

[<Fact>]
let ``constant behaves correctly`` () =
    Circuit.run (constant 1) [ (); (); () ] =! [ 1; 1; 1 ]

(* Circuit Mean *)

let mean =
    total &&& (constant 1. >>> total) >>> circuit (uncurry (/))

(* Circuit Mean Tests *)

[<Fact>]
let ``mean behaves correctly`` () =
    Circuit.run mean [ 0.; 10.; 2.; 3. ] =! [ 0.0; 5.0; 4.0; 3.75 ]