module Anat

(* Types

   Basic types, principally a wrapper type for the underlying kind of arrow,
   for example, a simple function, an async function, etc.

   Fuller inference happens statically based on the operation and the type
   defined by 'a. *)

type Arrow<'a> =
    | Arrow of 'a

(* Inference

   Static (compile time) inference for various functions in the (pseudo) type
   class Arrow (in fact in some of the implied classes which Arrow inhabits,
   but we don't have those hanging around already, so... *)

[<RequireQualifiedAccess>]
module Infer =

    (* Arrow

       Basic inference converting a valid function to an Arrow of that function,
       including a function which is already an Arrow being mapped through the
       identity function. *)

    type ArrowDefaults =
        | ArrowDefaults

        (* Arrows *)

        static member inline Arrow (a: Arrow<'a>) =
            a

        (* Functions *)

        static member inline Arrow (f: 'a -> 'b) =
            Arrow f

        (* Async Functions *)

        static member inline Arrow (f: 'a -> Async<'b>) =
            Arrow f


    let inline arrowDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Arrow: ^a -> ^b Arrow) a)

    let inline arrow (a: 'a) =
        arrowDefaults (a, ArrowDefaults)

    (* Compose *)

    type ComposeDefaults =
        | ComposeDefaults

        (* Functions *)

        static member inline Compose ((Arrow a1): Arrow<('a -> 'b)>) =
            fun ((Arrow a2): Arrow<('b -> 'c)>) ->
                Arrow (a1 >> a2)

        (* Async Functions *)

        static member inline Compose ((Arrow a1): Arrow<('a -> Async<'b>)>) =
            fun ((Arrow a2): Arrow<('b -> Async<'c>)>) ->
                Arrow (fun a -> async.Bind (a1 a, a2))

    let inline composeDefaults (a: ^a Arrow, _: ^defaults) =
        ((^a or ^defaults) : (static member Compose: ^a Arrow -> (^b Arrow -> ^c Arrow)) a)

    let inline compose (a: Arrow<'a>) =
        composeDefaults (a, ComposeDefaults)

(* Arrow

   The core public API for Arrows, following the F# convention of a module
   prefixed set of functions, rather than naked functions ala Haskell. Slightly
   more verbose, but more discoverable, especially in this case with probably
   unfamiliar operations.

   Some functions are renamed from their more familiar forms in the Hughes,
   etc. papers, for clarity or to provide non-symbolic equivalents (though
   symbolic operators are provided later). These are:

   - arr => arrow
   - >>> => compose *)

[<RequireQualifiedAccess>]
module Arrow =

    let inline arrow a =
        Infer.arrow a

    let inline compose a1 a2 =
        Infer.compose (arrow a1) (arrow a2)

(* Operators

   Symbolic operator forms of some of the more common Arrow functions,
   following the conventions defined in the papers by Hughes, etc. and adopted
   by Haskell (Control.Arrow, etc.).

   The Operators module is not opened by default, and must be opened explcitly
   to avoid namespace pollution, especially as some of the operators clash with
   default F# operators for bitwise operations - though these are probably not
   likely to crop up in close proximity... *)

module Operators =

    let inline (>>>) a1 a2 =
        Arrow.compose a1 a2











(* Temporary

   Make sure that syntax, inference, etc. still works as expected while we make
   some changes and extend things! To be pulled out to unit tests when there is
   some stability. *)

open Operators

(* Fixtures *)

let f1 : bool -> int =
    (function | true -> 1
              | _ -> 0)

let a1 : Arrow<(int -> string)> =
    (function | 1 -> "true"
              | _ -> "false") |> Arrow.arrow

(* Composition *)

let a2 : Arrow<(bool -> string)> =
    f1 >>> a1