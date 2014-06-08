(**
 * Functor to create type-aware shtream modules.  The base shtream
 * module {!Shtream} is indifferent to the element type.  The functor
 * {!AnyShtream.Make}, on the other hand, produces a module with shtream
 * functions that know how read shtream from and write shtreams to
 * channels without a user-supplied reader or printer function.
 *
 * Modules {!LineShtream} and {!StringShtream} are both created using
 * this functor, though some values in {!LineShtream} are specialized
 * further.
 *)

(** The output signature of the functor {!AnyShtream.Make}.
 * The shtream and coshtream types in the resulting module are
 * compatible with other applications of the functor and with {!Shtream}.
 *
 * When {!AnyShtream.Make} is applied to a structure [Elem] (having
 * signature {!ELEM}), the resulting module knows how to write
 * shtreams of type ['a Elem.elem Shtream.t] and read shtreams of type
 * [Elem.initial Elem.elem Shtream.t].  Functions in the resulting
 * module take several optional parameters whose defaults are
 * supplied by [Elem]:
 * - [?(reader : in_channel -> initial elem)] defaults to
 *   [Elem.reader ()].
 * - [?(parse : string -> initial elem)] defaults to [Elem.of_string ()].
 * - [?(show : 'a elem -> string)] defaults to [Elem.string_of ()].
 *
 * This signature is equivalent to {!Fitting.SHTREAM},
 * the input signature of the functor {!Fitting.Make}.
 *)

(** Build a new shtream module.  The {!ELEM}
 * parameter specifies how to read and print shtream elements. *)
module Make(E : S.Elem) : S.AnyShtream with module Elem = E
                                        and type 'a t = 'a Shtream.t
                                        and type 'a co_t = 'a Shtream.co_t
