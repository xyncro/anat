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

        static member inline Arrow (a: _ Arrow) =
            a

        (* Value Functions *)

        static member inline Arrow (f: ^a -> ^b when ^b : equality) =
            Arrow f

        (* Async Functions *)

        static member inline Arrow (f: ^a -> ^b Async) =
            Arrow f


    let inline arrowDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Arrow: ^a -> ^b Arrow) a)

    let inline arrow (a: ^a) =
        arrowDefaults (a, ArrowDefaults)

    (* Compose *)

    type ComposeDefaults =
        | ComposeDefaults

        (* Value Functions *)

        static member inline Compose (Arrow (a1: ^a -> ^b when ^b : equality)) =
            fun (Arrow (a2: ^b -> ^c when ^c : equality)) ->
                Arrow (a1 >> a2)

        (* Async Functions *)

        static member inline Compose ((Arrow a1): Arrow<(^a -> ^b Async)>) =
            fun ((Arrow a2): Arrow<(^b -> ^c Async)>) ->
                Arrow (fun a -> async.Bind (a1 a, a2))

    let inline composeDefaults (a: ^a Arrow, _: ^defaults) =
        ((^a or ^defaults) : (static member Compose: ^a Arrow -> (^b Arrow -> ^c Arrow)) a)

    let inline compose (a: Arrow<'a>) =
        composeDefaults (a, ComposeDefaults)

    (* Fanout *)

    type FanoutDefaults =
        | FanoutDefaults

        (* Functions *)

        static member inline Fanout ((Arrow a1): Arrow<('a -> 'b)>) =
            fun ((Arrow a2): Arrow<('a -> 'c)>) ->
                Arrow (fun a -> a1 a, a2 a)

        (* Async Functions *)

        static member inline Fanout ((Arrow a1): Arrow<('a -> Async<'b>)>) =
            fun ((Arrow a2): Arrow<('a -> Async<'c>)>) ->
                Arrow (fun a -> async {
                    let! b = a1 a
                    let! c = a2 a

                    return b, c })

    let inline fanoutDefaults (a: ^a Arrow, _: ^defaults) =
        ((^a or ^defaults) : (static member Fanout: ^a Arrow -> (^b Arrow -> ^c Arrow)) a)

    let inline fanout (a: Arrow<'a>) =
        fanoutDefaults (a, FanoutDefaults)

(* Arrow

   The core public API for Arrows, following the F# convention of a module
   prefixed set of functions, rather than naked functions ala Haskell. Slightly
   more verbose, but more discoverable, especially in this case with probably
   unfamiliar operations.

   Some functions are renamed from their more familiar forms in the Hughes,
   etc. papers, for clarity or to provide non-symbolic equivalents (though
   symbolic operators are provided later). These are:

   - arr => arrow
   - >>> => compose
   - &&& => fanout
   - *** => split *)

[<RequireQualifiedAccess>]
module Arrow =

    (* Basic Functions *)

    let run (Arrow a1) a =
        a1 a

    (* Inferred Functions *)

    let inline arrow a =
        Infer.arrow a

    let inline compose a1 a2 =
        Infer.compose (arrow a1) (arrow a2)

    let inline fanout a1 a2 =
        Infer.fanout (arrow a1) (arrow a2)


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

    let inline (&&&) a1 a2 =
        Arrow.fanout a1 a2






(* Temporary

   Make sure that syntax, inference, etc. still works as expected while we make
   some changes and extend things! To be pulled out to unit tests when there is
   some stability. *)

open Operators

(* Fixtures *)

let testAsync =
    fun (i: int) -> async.Return (2 * i)

//Async.

let f1 : bool -> int list =
    (function | true -> [ 1 ]
              | _ -> [ 0 ])

let f2 : int list -> string =
    (function | [ 1 ] -> "true"
              | _ -> "false")

let f3 : bool -> string =
    (function | true -> "true"
              | _ -> "false")

(* Composition *)

let a1 : Arrow<(bool -> string)> =
    f1 >>> f2

(* Fanout *)

let a2 : Arrow<(bool -> int list * string)> =
    f1 &&& f3