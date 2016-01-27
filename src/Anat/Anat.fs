module Anat

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

        (* Value Functions *)

        static member Arrow (f: 'a -> 'b when 'b : equality) =
            f

        (* Async Value Functions *)

        static member Arrow (f: 'a -> Async<'b>) =
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

        (* Value Functions *)

        static member Compose (f: 'a -> 'b when 'b : equality) =
            fun (g: 'b -> 'c when 'c : equality) ->
                f >> g

        (* Async Value Functions *)

        static member Compose (f: 'a -> Async<'b>) =
            fun (g: 'b -> Async<'c>) ->
                fun a ->
                    async.Bind (f a, g)

    let inline composeDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Compose: ^a -> (^b -> ^c)) a)

    let inline compose (a: 'a) =
        composeDefaults (a, ComposeDefaults)

    (* First

       Inferred application of an Arrow to the first of a pair of inputs,
       giving a pair of outputs. *)

    type FirstDefaults =
        | FirstDefaults

        (* Value Functions *)

        static member First (f: 'a -> 'b when 'b : equality) =
            fun (a, b) ->
                f a, b

        (* Async Value Functions *)

        static member First (f: 'a -> Async<'c>) =
            fun (a, b) ->
                async.Bind (f a, fun c ->
                    async.Return (c, b))

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

        (* Value Functions *)

        static member Second (f: 'a -> 'b when 'b : equality) =
            fun (a, b) ->
                a, f b

        (* Async Value Functions *)

        static member Second (f: 'b -> Async<'c>) =
            fun (a, b) ->
                async.Bind (f b, fun c ->
                    async.Return (a, c))

    let inline secondDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Second: ^a -> ^b) a)

    let inline second (a: 'a) =
        secondDefaults (a, SecondDefaults)

    (* Fanout

       Creates an Arrow applying a pair of Arrow functions to a single input
       value, returning a pair of output values. *)

    type FanoutDefaults =
        | FanoutDefaults

        (* Value Functions *)

        static member Fanout (f: 'a -> 'b when 'b : equality) =
            fun (g: 'a -> 'c when 'c : equality) ->
                fun a ->
                    f a, g a

        (* Async Value Functions *)

        static member Fanout (f: 'a -> Async<'b>) =
            fun (g: 'a -> Async<'c>) ->
                fun a ->
                    async.Bind (f a, fun b ->
                        async.Bind (g a, fun c ->
                            async.Return (b, c)))

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

        (* Value Functions *)

        static member Split (f: 'a -> 'c when 'c : equality) =
            fun (g: 'b -> 'd when 'd : equality) ->
                fun (a, b) ->
                    f a, g b

        (* Async Value Functions *)

        static member Split (f: 'a -> Async<'c>) =
            fun (g: 'b -> Async<'d>) ->
                fun (a, b) ->
                    async.Bind (f a, fun c ->
                        async.Bind (g b, fun d ->
                            async.Return (c, d)))

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
