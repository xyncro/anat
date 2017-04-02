/// Provides common operations on `Arrow`-like values
module Anat

open System

(* Inference

   Static (compile time) inference for various functions in the (pseudo) type
   class Arrow (in fact in some of the implied classes which Arrow inhabits,
   but we don't have those hanging around already, so... *)

/// Statc (compile-time) inference for various functions in the (pseudo)
/// type-class `Arrow`.
[<RequireQualifiedAccess>]
module Infer =

    /// Default `arrow` implementations for `Arrow`-like values.
    type ArrowDefaults =
        /// Inference target for default `arrow` implementations
        | ArrowDefaults

        /// Maps a function to an `Arrow` through the identity function.
        static member Arrow (f: _ -> _) =
            f

        /// Maps a `Func` to an `Arrow` through the identity function.
        static member Arrow (f: Func<_,_>) =
            f

    /// Infers an `arrow` implementation from an `Arrow`-like value using the
    /// provided default implementation host.
    let inline arrowDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Arrow: ^a -> ^b) a)

    /// Infers an `arrow` implementation from an `Arrow`-like value. Default
    /// implementations are provided for:
    ///
    /// * `'a -> 'b`
    /// * `Func<'a,'b>`
    let inline arrow (a: ^a) =
        arrowDefaults (a, ArrowDefaults)

    /// Default `compose` implementations for the composition of two `Arrow`s.
    type ComposeDefaults =
        /// Inference target for default `compose` implementations
        | ComposeDefaults

        /// Composes two functionss into a new `Arrow`.
        static member Compose (f: _ -> _) =
            fun (g: _ -> _) -> f >> g

        /// Composes two `Func`s into a new `Arrow`.
        static member Compose (f: Func<_,_>) =
            fun (g: Func<_,_>) -> Func<_,_> (f.Invoke >> g.Invoke)

    /// Infers a `compose` implementation from an `Arrow`-like value using the
    /// provided default implementation host.
    let inline composeDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Compose: ^a -> (^b -> ^c)) a)

    /// Infers a `compose` implementation from an `Arrow`-like value. Default
    /// implementations are provided for:
    ///
    /// * `'a -> 'b`
    /// * `Func<'a,'b>`
    let inline compose (a: 'a) =
        composeDefaults (a, ComposeDefaults)

    /// Default `first` implementations for the application of an `Arrow` to
    /// the first of a pair of inputs, giving a pair of outputs.
    type FirstDefaults =
        /// Inference target for default `first` implementations
        | FirstDefaults

        /// Applies a function to the first of a pair of inputs, giving a pair
        /// of outputs.
        static member First (f: _ -> _) =
            fun (a, b) -> f a, b

        /// Applies a `Func` to the first of a pair of inputs, giving a pair
        /// of outputs.
        static member First (f: Func<_,_>) =
            Func<_*_,_*_> (fun (a, b) -> f.Invoke a, b)

    (* Functions *)

    /// Infers a `first` implementation from an `Arrow`-like value using the
    /// provided default implementation host.
    let inline firstDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member First: ^a -> ^b) a)

    /// Infers a `first` implementation from an `Arrow`-like value. Default
    /// implementations are provided for:
    ///
    /// * `'a -> 'b`
    /// * `Func<'a,'b>`
    let inline first (a: 'a) =
        firstDefaults (a, FirstDefaults)

    /// Default `second` implementations for the application of an `Arrow` to
    /// the second of a pair of inputs, giving a pair of outputs.
    type SecondDefaults =
        /// Inference target for default `second` implementations
        | SecondDefaults

        /// Applies a function to the second of a pair of inputs, giving a pair
        /// of outputs.
        static member Second (f: _ -> _) =
            fun (a, b) -> a, f b

        /// Applies a `Func` to the second of a pair of inputs, giving a pair
        /// of outputs.
        static member Second (f: Func<_,_>) =
            Func<_*_,_*_> (fun (a, b) -> a, f.Invoke b)

    /// Infers a `second` implementation from an `Arrow`-like value using the
    /// provided default implementation host.
    let inline secondDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Second: ^a -> ^b) a)

    /// Infers a `second` implementation from an `Arrow`-like value. Default
    /// implementations are provided for:
    ///
    /// * `'a -> 'b`
    /// * `Func<'a,'b>`
    let inline second (a: 'a) =
        secondDefaults (a, SecondDefaults)

    /// Default `fanout` implementations for the application of a pair of
    /// `Arrow`s to a single input value, returning a pair of output values.
    type FanoutDefaults =
        /// Inference target for default `fanout` implementations
        | FanoutDefaults

        /// Applies a pair of functions to a single input value, returning a
        /// pair of output values.
        static member Fanout (f: _ -> _) =
            fun (g: _ -> _) -> fun a -> f a, g a

        /// Applies a pair of `Func`s to a single input value, returning a
        /// pair of output values.
        static member Fanout (f: Func<_,_>) =
            fun (g: Func<_,_>) -> Func<_,_*_> (fun a -> f.Invoke a, g.Invoke a)

    /// Infers a `fanout` implementation from an `Arrow`-like value using the
    /// provided default implementation host.
    let inline fanoutDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Fanout: ^a -> (^b -> ^c)) a)

    /// Infers a `fanout` implementation from an `Arrow`-like value. Default
    /// implementations are provided for:
    ///
    /// * `'a -> 'b`
    /// * `Func<'a,'b>`
    let inline fanout (a: 'a) =
        fanoutDefaults (a, FanoutDefaults)

    /// Default `split` implementations for the application of a pair of
    /// `Arrow`s to a pair of input values, giving a pair of output values.
    ///
    /// Commonly known as a bimap in arrow theory.
    type SplitDefaults =
        /// Inference target for default `split` implementations
        | SplitDefaults

        /// Applies a pair of functions to a pair of input values, giving a
        /// pair of output values.
        static member Split (f: _ -> _) =
            fun (g: _ -> _) -> fun (a, b) -> f a, g b

        /// Applies a pair of `Func`s to a pair of input values, giving a
        /// pair of output values.
        static member Split (f: Func<_,_>) =
            fun (g: Func<_,_>) -> Func<_*_,_*_> (fun (a, b) -> f.Invoke a, g.Invoke b)

    /// Infers a `split` implementation from an `Arrow`-like value using the
    /// provided default implementation host.
    let inline splitDefaults (a: ^a, _: ^defaults) =
        ((^a or ^defaults) : (static member Split: ^a -> (^b -> ^c)) a)

    /// Infers a `split` implementation from an `Arrow`-like value. Default
    /// implementations are provided for:
    ///
    /// * `'a -> 'b`
    /// * `Func<'a,'b>`
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

/// Operations for working with `Arrow`s.
[<RequireQualifiedAccess>]
module Arrow =

    (* Basic Functions *)

    /// Provides the value in an `Arrow` instance
    let run a =
        a

    (* Inferred Functions *)

    /// Instantiates an `Arrow` from an `Arrow`-like value.
    ///
    /// See also: `Infer.arrow`
    let inline arrow f =
        Infer.arrow f

    /// Creates a new `Arrow` from the composition of two `Arrow`-like
    /// instances.
    ///
    /// See also: `Infer.compose`
    let inline compose f g =
        Infer.compose (arrow f) (arrow g)

    /// Applies an `Arrow` to the first of a pair of inputs, giving a pair of
    /// outputs.
    ///
    /// See also: `Infer.first`
    let inline first f =
        Infer.first (arrow f)

    /// Applies an `Arrow` to the second of a pair of inputs, giving a pair of
    /// outputs.
    ///
    /// See also: `Infer.second`
    let inline second f =
        Infer.second (arrow f)

    /// Creates an `Arrow` applying a pair of `Arrow` functions to a single
    /// input value, returning a pair of output values.
    ///
    /// See also: `Infer.fanout`
    let inline fanout f g =
        Infer.fanout (arrow f) (arrow g)

    /// Creates an `Arrow` applying a pair of `Arrow` functions to a pair of
    /// input values, giving a pair of output values.
    ///
    /// Commonly known as bimap in more general theory.
    ///
    /// See also: `Infer.split`
    let inline split f g =
        Infer.split (arrow f) (arrow g)

/// Symbolic operator forms of some of the more common Arrow functions,
/// following the conventions defined in the papers by Hughes, etc. and adopted
/// by Haskell (Control.Arrow, etc.).

/// The Operators module is not opened by default, and must be opened explcitly
/// to avoid namespace pollution, especially as some of the operators clash with
/// default F# operators for bitwise operations - though these are probably not
/// likely to crop up in close proximity...
module Operators =

    (* Arrow Operators *)
    /// Creates a new `Arrow` from the composition of two `Arrow`-like
    /// instances.
    ///
    /// Equivalent to `Arrow.compose`
    let inline ( >>> ) f g =
        Arrow.compose f g

    /// Creates an Arrow applying a pair of Arrow functions to a single input
    /// value, returning a pair of output values.
    ///
    /// Equivalent to `Arrow.fanout`
    let inline ( &&& ) f g =
        Arrow.fanout f g


    /// Creates an `Arrow` applying a pair of `Arrow` functions to a pair of
    /// input values, giving a pair of output values.
    ///
    /// Commonly known as bimap in more general theory.
    ///
    /// Equivalent to `Arrow.split`
    let inline ( *** ) f g =
        Arrow.split f g

    (* Arrow Functions

       Using the operators also introduces the unqualified functions first and
       second in to scope, to aid in more concise and idiomatic usage of Arrow
       functions. *)

    /// Applies an `Arrow` to the first of a pair of inputs, giving a pair of
    /// outputs.
    ///
    /// Equivalent to `Arrow.first`
    let inline first f =
        Arrow.first f

    /// Applies an `Arrow` to the second of a pair of inputs, giving a pair of
    /// outputs.
    ///
    /// Equivalent to `Arrow.second`
    let inline second f =
        Arrow.second f
