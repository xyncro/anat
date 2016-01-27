module Anat

open System

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

        (* _ -> _ *)

        static member Arrow (f: _ -> _) =
            f

        (* Func<_,_> *)

        static member Arrow (f: Func<_,_>) =
            f

    let inline arrowDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Arrow: ^a -> ^b) a)

    let inline arrow (a: ^a) =
        arrowDefaults (a, ArrowDefaults)

    (* Compose

       Inferred composition of two Arrows, where the composition mechanism
       will vary based on the Arrow type (composition of plain/async/etc.
       functions varies the implementation). *)

    type ComposeDefaults =
        | ComposeDefaults

        (* _ -> _ *)

        static member Compose (f: _ -> _) =
            fun (g: _ -> _) -> f >> g

        (* Func<_,_> *)

        static member Compose (f: Func<_,_>) =
            fun (g: Func<_,_>) -> Func<_,_> (f.Invoke >> g.Invoke)

    let inline composeDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Compose: ^a -> (^b -> ^c)) a)

    let inline compose (a: 'a) =
        composeDefaults (a, ComposeDefaults)

    (* First

       Inferred application of an Arrow to the first of a pair of inputs,
       giving a pair of outputs. *)

    type FirstDefaults =
        | FirstDefaults

        (* _ -> _ *)

        static member First (f: _ -> _) =
            fun (a, b) -> f a, b

        (* Func<_,_> *)

        static member First (f: Func<_,_>) =
            Func<_*_,_*_> (fun (a, b) -> f.Invoke a, b)

    (* Functions *)

    let inline firstDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member First: ^a -> ^b) a)

    let inline first (a: 'a) =
        firstDefaults (a, FirstDefaults)

    (* Second

       Inferred application of an Arrow to the second of a pair of inputs,
       giving a pair of outputs. *)

    type SecondDefaults =
        | SecondDefaults

        (* _ -> _ *)

        static member Second (f: _ -> _) =
            fun (a, b) -> a, f b

        (* Func<_,_> *)

        static member Second (f: Func<_,_>) =
            Func<_*_,_*_> (fun (a, b) -> a, f.Invoke b)

    let inline secondDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Second: ^a -> ^b) a)

    let inline second (a: 'a) =
        secondDefaults (a, SecondDefaults)

    (* Fanout

       Creates an Arrow applying a pair of Arrow functions to a single input
       value, returning a pair of output values. *)

    type FanoutDefaults =
        | FanoutDefaults

        (* _ -> _ *)

        static member Fanout (f: _ -> _) =
            fun (g: _ -> _) -> fun a -> f a, g a

        (* Func<_,_> *)

        static member Fanout (f: Func<_,_>) =
            fun (g: Func<_,_>) -> Func<_,_*_> (fun a -> f.Invoke a, g.Invoke a)

    let inline fanoutDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Fanout: ^a -> (^b -> ^c)) a)

    let inline fanout (a: 'a) =
        fanoutDefaults (a, FanoutDefaults)

    (* Split

       Creates an Arrow applying a pair of Arrow functions to a pair of input
       values, giving a pair of output values (commonly known as bimap in more
       general theory). *)

    type SplitDefaults =
        | SplitDefaults

        (* _ -> _ *)

        static member Split (f: _ -> _) =
            fun (g: _ -> _) -> fun (a, b) -> f a, g b

        (* Func<_,_> *)

        static member Split (f: Func<_,_>) =
            fun (g: Func<_,_>) -> Func<_*_,_*_> (fun (a, b) -> f.Invoke a, g.Invoke b)

    let inline splitDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Split: ^a -> (^b -> ^c)) a)

    let inline split (a: 'a) =
        splitDefaults (a, SplitDefaults)

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

    let run a =
        a

    (* Inferred Functions *)

    let inline arrow f =
        Infer.arrow f

    let inline compose f g =
        Infer.compose (arrow f) (arrow g)

    let inline first f =
        Infer.first (arrow f)

    let inline second f =
        Infer.second (arrow f)

    let inline fanout f g =
        Infer.fanout (arrow f) (arrow g)

    let inline split f g =
        Infer.split (arrow f) (arrow g)

(* Operators

   Symbolic operator forms of some of the more common Arrow functions,
   following the conventions defined in the papers by Hughes, etc. and adopted
   by Haskell (Control.Arrow, etc.).

   The Operators module is not opened by default, and must be opened explcitly
   to avoid namespace pollution, especially as some of the operators clash with
   default F# operators for bitwise operations - though these are probably not
   likely to crop up in close proximity... *)

module Operators =

    (* Arrow Operators *)

    let inline ( >>> ) f g =
        Arrow.compose f g

    let inline ( &&& ) f g =
        Arrow.fanout f g

    let inline ( *** ) f g =
        Arrow.split f g

    (* Arrow Functions

       Using the operators also introduces the unqualified functions first and
       second in to scope, to aid in more concise and idiomatic usage of Arrow
       functions. *)

    let inline first f =
        Arrow.first f

    let inline second f =
        Arrow.second f
