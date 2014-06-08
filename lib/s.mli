module type Elem = sig
  (** The element type may be polymorphic, in which case the conversion
   * of elements to strings must handle any element.  The conversion
   * from strings (or reading from channels) is monomorphic, returning
   * shtream elements of a particular type.
  *)

  type 'a elem
  (** The element type for the resulting shtream module.  This type is
   * parameterized so that a shtream module might handle a family of
   * types.  The function {!string_of} needs handle ['a elem] for any
   * ['a]. *)
  type initial
  (** The parameter to {!elem} for values returned by conversions from
   * strings.  That is, [initial elem] is the type of shtream elements when
   * first read from a string or channel. *)

  val reader    : unit -> in_channel -> initial elem
  (** Make a reader of shtream elements.  The reader may be stateful;
   * a new one will be instantiated for each shtream. *)
  val of_string : unit -> string -> initial elem
  (** Make a parser of shtream elements.  The parser may be stateful;
   * a new one will be instantiated for each shtream. *)
  val string_of : unit -> 'a elem -> string
  (** Make a convertor of shtream elements to strings.  The resulting
   * function may be stateful; a new one will be instantiated for
   * shtream output operation. *)
end

module type Shtream = sig
  (** Raised on attempts to retrieve elements from an empty
   * {!t}.  Callbacks such as the argument to {!from}
   * or {!map} may raise this to indicate the end of the
   * shtream they are producing. *)
  exception Failure

  (** A shtream with elements of type ['a] *)
  type 'a t

  (** Raised if a coshtream is no longer accepting data.  This could
   * happen, for example, were the shtream-consuming function to
   * return before exhausting the shtream. *)
  exception CoFailure

  (** A coshtream accepting elements of type ['a] *)
  type 'a co_t
  (** {2 Basic Shtream Operations} *)

  val from        : (int -> 'a option) -> 'a t
  (** Generate a shtream by repeatedly calling a function.
   * The function is called with the natural numbers starting from
   * 0, and should return
   * [Some v] to produce [v] or [None] to end the shtream.  The
   * function may also use {!shtream_errors}.
   * *)
  val close       : 'a t -> unit
  (** Free resources (such as channels) associated with a
   * shtream.  While channels will eventually be reclaimed by the
   * garbage collector, the file descriptor table could potentially
   * fill up without a collection.
  *)

  val of_list     : 'a list -> 'a t
  (** Construct a shtream from the elements of a list. *)
  val list_of     : 'a t -> 'a list
  (** Return the contents of a shtream as a list.  If the shtream is
   * infinite, this function won't terminate. *)
  val of_stream   : 'a Stream.t -> 'a t 
  (** Convert a {i standard library} [Stream.t] to a shtream. *)
  val stream_of   : 'a t -> 'a Stream.t
  (** Convert a shtream to a {i standard library} [Stream.t]. *)

  val npeek       : ?n:int -> 'a t -> 'a list
  (** Get the first [n] (default [1]) elements of a shtream.
   * If there are fewer than [n] elements remaining, returns only
   * those.
   * Leaves the elements in the shtream. *)
  val peek        : ?n:int -> 'a t -> 'a option
  (** Get the [n]th (default [0]th) element of a shtream.  Returns
   * [Some v] if [v] is the [n]th zero-indexed element, and [None] if
   * the shtream has [n] or fewer elements remaining.  Leaves
   * elements in the shtream. *)
  val empty       : 'a t -> unit
  (** Return [()] if the shtream is empty.  Raises {!Failure} if
   * elements remain. *)
  val is_empty    : 'a t -> bool
  (** Is a shtream empty?  This may cause one element of the shtream
   * to be evaluted; for example, it may read an element from a
   * channel to check for [End_of_file].  The element is not
   * discarded. *)
  val status      : 'a t -> Proc.status option
  (** Retrieve the termination status of an exhausted shtream.  If the
   * shtream still has data, returns [None].  Otherwise, [Some
   * (Proc.WEXITED 0)] indicates normal termination and [Some n] for
   * non-zero [n] indicates abnormal termination.  If a shtream was made
   * from an external process (via {!Shtream.of_command}, for example),
   * then [n] is the exit code of the process.  (If the process closes its
   * output and continues running, the shtream will terminate with [Some
   * 0] rather than wait for the process.)
  *)

  val junk        : ?n:int -> 'a t -> unit
  (** Discard the first [n] (default 1) elements of a shtream.  If
   * fewer than [n] remain, discards them all. *)
  val next        : 'a t -> 'a
  (** Return and discard the next element of a shtream.  If the
   * shtream is empty, raises {!Failure}. *)
  val next'       : 'a t -> 'a option
  (** Return and discard the next element of a shtream.  If the
   * shtream is empty, returns [None]. *)

  (** {2 Shtream Construction and Observation} *)

  val iter        : ('a -> unit) -> 'a t -> unit
  (** Apply a function to each element of a shtream in turn.  This
   * function exhausts the shtream. *)
  val filter      : ('a -> bool) -> 'a t -> 'a t
  (** Lazily filter a shtream according to a predicate.
   * [Shtream.filter pred s] returns a new
   * shtream containing all the elements of [s] that satisfy [pred]. 
   * The order of the elements is preserved, and [s] becomes
   * invalid. *)
  val map         : ('a -> 'b) -> 'a t -> 'b t
  (** Lazily map a function over the elements of a shtream.  The old shtream
   * becomes invalid. *)
  val concat_map  : ('a -> 'b list) -> 'a t -> 'b t
  (** Lazily map a function over a shtream, concatenating the results.
   * [Shtream.concat_map f s] applies [f] to each element of [s] in
   * turn, concatenating the resulting lists to form a new shtream.
   * The old shtream [s] becomes invalid. *)
  val fold_left   : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Eagerly fold a function over a shtream.
   * [Shtream.fold_left f z s] applies the function [f] to each
   * element of [s] in turn, with an accumulating parameter
   * that starts at [z], and then returns the accumulated value.
   * Isomorphic to [List.fold_left]. *)
  val fold_right  : ('a -> 'b Lazy.t -> 'b) -> 'a t -> 'b -> 'b
  (** Lazily fold a function over a shtream.
   * [Shtream.fold_right f s z] applies [f] to each element
   * of [s] and a lazy value that, when forced, returns the
   * rest of the fold.  At the end of the shtream, the lazy value
   * returns [z].  Isomorphic to call-by-need [List.fold_right].
  *)

  val nil         : unit -> 'a t
  (** Return the empty shtream. *)
  val insert      : 'a -> 'a t -> unit
  (** Prepend an element to a shtream. *)
  val cons        : 'a -> 'a t -> 'a t
  (** Prepend an element to a shtream and return the shtream. *)
  val append      : 'a t -> 'a t -> 'a t
  (** Append two shtreams.  [Shtream.append s1 s2] returns a shtream
   * that first produces the elements of [s1], and should [s1] be
   * exhausted, then produces the elements of [s2].  This operation
   * invalidates both [s1] and [s2].
  *)

  (** {3:shtream_errors Shtream Error Handling } *)

  (** Because a shtream may be used far from its creation site, the
   * client code of a shtream may be ill prepared to handle any
   * exceptions the shtream might raise.  To this end, we provide a
   * simple API for signaling and handling shtream errors.
   *
   * Here are several functions that a shtream generator (for
   * example, the callback given to {!from} or a reader) may call to
   * signal a condition:
   * *)

  val try_again   : unit   -> 'a
  (** Indicates that the generating function cannot produce a value
   * now.  The shtream evaluator should call it again. *)
  val warn        : ('a, unit, string, 'b) format4 -> 'a
  (** Indicates that the generating function has encountered a
   * problem.  The error handler will decide what to do. *)
  val fail_with   : Proc.status -> 'a
  (** Indicates that a shtream should terminate with the given error
   * code.  Calling [fail_with (Proc.WEXITED 0)] is equivalent to raising
   * {!Failure}.  Calling [fail_with st] results in an exit status of
   * [st].
  *)

  type error_handler = [`Warning of string | `Exception of exn] -> unit 
  (** The type for a shtream error handler.  If a shtream generator
   * calls [warn s] then the error handler receives [`Warning s].  If
   * a shtream generator raises an exception [e], then the error_handler
   * receives [`Exception e].
  *)

  val current_error_handler  : error_handler ref
  (** Reference containing the shtream error handler.  This may be
   * changed to one of the predefined error handling functions such as
   * {!ignore_errors} or a user-defined function. *)

  val ignore_errors          : error_handler
  (** Ignore errors and continue evaluating the shtream. *)
  val warn_on_errors         : error_handler
  (** Print a warning on [stderr] and continue evaluating the shtream. *)
  val die_on_errors          : error_handler
  (** Print a warning on [stderr] terminate the shtream. *)
  val die_silently_on_errors : error_handler
  (** Just terminate the shtream. *)

  (** {2 Coshtreams}
   *
   * A shtream is a source of typed data; a coshtream, of course, is a
   * sink.  Given a function [f] that consumes a shtream, {!coshtream_of}
   * returns a coshtream handle that can be used to supply shtream
   * elements to [f] asynchronously.
   *
   * Because coshtreams work by calling [f] in a child process, they
   * must marshal shtream data over a pipe; thus, they work only if
   * the element type is serializable using [Marshal].  Moreover,
   * internal side effects in [f], such as mutating a ref cell, will not
   * be visible, since [f] is called in a separate process.
   * *)

  val coshtream_of : ?procref:Channel.procref -> ('a t -> 'b) -> 'a co_t
  (** Construct a coshtream from a shtream-consuming function.
   * [coshtream_of f] spawns a child process in which it invokes [f],
   * and relays values sent to the resulting coshtream into the
   * shtream given to [f].  If [procref] is provided, the child
   * {!Proc.t} is stashed therein.
  *)
  val conil        : unit -> 'a co_t
  (** Returns the empty coshtream.  The empty coshtream accepts
   * data ad infinitum. *)

  val conext       : 'a co_t -> 'a -> unit
  (** Send a value to a coshtream. *)
  val coclose      : 'a co_t -> unit
  (** Indicate the end of a coshtream.  The shtream consuming function
   * will see this as the end of the shtream. *)

  val annihilate   : 'a t -> 'a co_t -> unit
  (** Send the contents of a shtream to a coshtream.  This leaves
   * the coshtream open, and if the coshtream stops accepting data
   * before the shtream is exhausted, data {i may} remain in the shtream.
  *)

  (** {2 Low level (scary stuff)} *)

  (** Generate a shtream from a function, and include a close
   * operation.  This is like {!from}, except that the function
   * returns ['a] rather than ['a option], and must signal the end of
   * the shtream by raising {!Failure}.  If [close] is provided, it
   * will be called when the resulting shtream is closed; this could
   * be useful to free resources associated with the shtream.
  *)
  val from_low    : ?close:(unit -> unit) -> (int -> 'a) -> 'a t

  (** Invalidate a shtream and return a copy.  [claim s] returns
   * a shtream behaving identically to [s], while modifying [s] to
   * contain no more data.  This is useful for functions that want to
   * lay claim to a shtream argument while consuming it lazily.
  *)
  val claim       : 'a t -> 'a t

  (** Change the channel reader of a shtream.  If the given shtream is
   * a channel-based shtream (made with {!Shtream.of_channel}, for
   * example), this causes it to begin using the supplied reader to
   * produce shtream elements.
  *)
  val set_reader  : 'a t -> (in_channel -> 'a) -> unit

  (** Possibly change the channel reader of a shtream.  This is like
   * {!set_reader}, but it only changes the reader if the current
   * reader was defaulted rather than supplied by the user.  For
   * example, if a shtream is created with {!AnyShtream.ANYSHTREAM.of_channel}
   * and the user does not supply its option [?reader] argument, then
   * the reader may be changed by the system using {!hint_reader};
   * but if the user explictly specifies a reader, then {!hint_reader}
   * has no effect.
  *)
  val hint_reader : 'a t -> Reader.t -> unit

  type protector = Util.protector
  (** Alias for {!Util.protector} *)
  val add_protection : protector -> 'a t -> unit
  (** Add advice to adjust parameters while evaluating a shtream.  A
   * {!protector} is a function that, given a thunk, must return the value
   * of the thunk, but may perform some preparation first or cleanup
   * afterward.  Adding a protector to a shtream means that any internal
   * shtream evalution will be performed in the dynamic context of the
   * protector.  If more than one protector has been added to a shtream,
   * they will be performed with the newest protector on the outside.
  *)
  val add_cleanup : (unit -> unit) -> 'a t -> unit
  (** Add advice for closing a shtream.  This function adds a thunk to
   * be performed when a shtream is closed.  If more than one close action
   * has been added, they will be performed in order from oldest to
   * newest.
  *)
end

module type ELEM = Elem

(** The result of {!AnyShtream.Make} contains all the type-indifferent
 * shtream operations from {!Shtream}. *)

module type AnyShtream = sig

  include Shtream with type 'a t = 'a Shtream.t
                   and type 'a co_t = 'a Shtream.co_t

  (** Access to the underlying element type and operations. *)
  module Elem : Elem

  type 'a elem = 'a Elem.elem
  (** Alias for {!ELEM.elem} *)
  type initial = Elem.initial
  (** Alias for {!ELEM.initial} *)

  (** Construct an [initial elem] reader from a record reader.
   * Functions such as {!of_channel} and {!of_program} take a function
   * of the type returned here.
  *)
  val elem_reader : Reader.t -> (in_channel -> initial elem)

  (** Write the entire contents of a shtream on a channel.
   * For each element [x] of the shtream, it prints [init x], then
   * [show x], and then [term x] on the channel, and then flushes the
   * channel.
   * @param channel default = [stdout]
   * @param init default = [fun _ -> ""]
   * @param show default = [Elem.string_of ()]
   * @param term default = [fun _ -> "\n"]
  *)
  val output      : ?channel:out_channel ->
    ?init:('a elem -> string) ->
    ?term:('a elem -> string) ->
    ?show:('a elem -> string) ->
    'a elem t ->
    unit

  (** Construct an [in_channel] from the data in a
   * shtream.  If forking a child is necessary (see
   * {!Shtream.channel_of}), then the optional
   * parameter [?before] (resp. [?after]) is called in the child
   * before (resp. after) printing the shtream; anything printed on
   * [stdout] by [?before] ([?after]) appears in the resultant
   * [in_channel] before (after) the shtream data.
   *
   * The remaining arguments are as for {!output}.
  *)
  val channel_of  : ?procref:Channel.procref ->
    ?before:(unit -> unit) ->
    ?after:(unit -> unit) ->
    ?init:('a elem -> string) ->
    ?term:('a elem -> string) ->
    ?show:('a elem -> string) ->
    'a elem t -> in_channel

  (** Convert a shtream to a list of strings, using [?show]. *)
  val string_list_of      : ?show:('a elem -> string) ->
    'a elem t -> string list

  (** Convert a shtream to a {i standard library} [Stream.t] of
   * strings, using [?show]. *)
  val string_stream_of    : ?show:('a elem -> string) ->
    'a elem t -> string Stream.t

  (** Read a shtream from a channel, using [?reader]. *)
  val of_channel  : ?reader:(in_channel -> initial elem) ->
    in_channel -> initial elem t

  (** Read a shtream from a file, using [?reader]. *)
  val of_file     : ?reader:(in_channel -> initial elem) ->
    string -> initial elem t

  (** Read a shtream from the output of a command, using [?reader].
   * If [?procref] is given, stash the {!Proc.t}; if [?dups]
   * is given, perform the dups in the child process. *)
  val of_command  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    string ->
    initial elem t

  (** Read a shtream from the output of a process, using [?reader].
   * If [?procref] is given, stash the {!Proc.t}; if [?dups]
   * is given, perform the dups in the child process. *)
  val of_program  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    ?path:bool -> string -> ?argv0:string -> string list ->
    initial elem t

  (** Read a shtream from the output of a thunk, using [?reader].
   * If [?procref] is given, stash the {!Proc.t}; if [?dups]
   * is given, perform the dups in the child process. *)
  val of_thunk  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    (unit -> unit) ->
    initial elem t

  (** Construct a shtream from a list of strings, using [?parse]. *)
  val of_string_list   : ?parse:(string -> initial elem) ->
    string list -> initial elem t

  (** Construct a shtream from a {i standard
   * library} [Stream.t] of strings, using [?parse]. *)
  val of_string_stream : ?parse:(string -> initial elem) ->
    string Stream.t -> initial elem t
end

module type Fitting = sig
  open Channel
  open Channel.Dup

  (** A fitting that consumes values of type ['a] and produces
   * values of type ['b].  *)
  type 'a t

  type 'a elem
  (**
   * This is the type of elements that fittings know how to write to
   * external processes.  This type comes from the functor
   * parameter {!SHTREAM}.
  *)
  type initial

  type 'a shtream

  type 'a coshtream

  (** Alias for {!Channel.procref} *)
  type procref   = Channel.procref

  (** Alias for {!initial} {!elem} *)
  type text = initial elem

  (** {3 Values} *)

  (** Connect the output of one fitting to the input of another.
   * This is the most basic fitting combinator, and bears introduction
   * before the full variety of fitting components. *)
  val ( -| )     : ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

  val pipe       : ('i -> 'm) t -> ('m -> 'o) t -> ('i -> 'o) t
  (** Alias for {!(-|)} *)

  (** {4 Basic Fitting Constructors} *)

  (** {5 Producers}
   *
   * Producers are useful for starting off pipelines.
  *)

  val from_file   : string -> ('i -> text) t
  (** Produce the contents of a file.
   * [from_file file -| fitting] is like {v     % fitting < file v} *)
  val from_null   : ('i -> text) t
  (** Produce nothing.  
   * [from_null -| fitting] is like {v     % fitting < /dev/null v} *)
  val from_gen    : Channel.dup_in_source -> ('i -> text) t
  (** Produce the contents of a {!Channel.dup_in_source}. *)
  val from_shtream: 'o shtream -> ('i -> 'o) t
  (** Produce the contents of a shtream. *)

  (** {5 Consumers}
   *
   * Consumers are useful for ending pipelines.
   * *)

  val to_file     : ?clobber:Channel.clobber_spec ->
    string -> ('i elem -> 'o) t
  (** Redirect standard output to a file.
   * See {!Channel.clobber_spec} for open modes.
   * [fitting -| to_file file] is like {v     % fitting > file v} *)
  val to_null     : ('i elem -> 'o) t
  (** Redirect standard output nowhere.
   * [fitting -| to_null] is like {v     % fitting > /dev/null v} *)
  val to_stderr   : ('i elem -> 'o) t
  (** Redirect standard output to standard error.
      [fitting -| to_stderr] is like {v     % fitting >&2 v} *)
  val to_gen      : Channel.dup_out_source -> ('i elem -> 'o) t
  (** Redirect standard output to {!Channel.dup_out_source}. *)
  val to_coshtream: 'i coshtream -> ('i -> 'o) t
  (** Redirect output to a coshtream.  (A coshtream is a consumer
   * in another process.) *)

  (** {5 Transformers} *)

  (** Run an external command as a fitting.  The fitting's input is
   * connected to the command's standard input and the fitting's output
   * to the command's standard output.  This runs the command in the
   * shell, in the style of {!Channel.open_command}. *)
  val command     : string -> ('i elem -> text) t

  (** Run an external program as a fitting.  The fitting's input is
   * connected to the program's standard input and the fitting's output
   * to the program's standard output.  This runs the program in the
   * shell, in the style of {!Channel.open_program}. *)
  val program     : ?path:bool -> string -> ?argv0:string -> string list ->
    ('i elem -> text) t

  (** Run a thunk as a fitting.  The thunk is run in a child
   * process whose standard input and output are connected to the
   * fitting's input and output. *)
  val thunk       : (unit -> unit) -> ('i elem -> text) t

  (** Map each element according to a function.
   * This lifts a function on elements into a fitting component. *)
  val sed         : ('i -> 'o) -> ('i -> 'o) t

  (** Filter the input according to a predicate. *)
  val grep        : ('i -> bool) -> ('i -> 'i) t

  (** Transform the input according to a function on shtreams. *)
  val trans       : ('i shtream -> 'o shtream) -> ('i -> 'o) t

  (** Like {!sed} with a lift from strings. *)
  val sed_string  : (string -> 'o)   -> ('i elem -> 'o) t

  (** Filter the input according to a string predicate. *)
  val grep_string : (string -> bool) -> ('i elem -> 'i elem) t

  (** {4 Fitting Combinators} *)

  val ( /</ )     : (text -> 'o) t -> dup_spec -> (text -> 'o) t
  (** Redirect some inputs to a fitting.  [fitting /</ dups]
   * performs the redirections specifed by [dups] for the extent
   * of [fitting].  For example,
   * [fitting /</ \[ 4 %<& 0; 0 %< "file" \]] is like
   * {v     % fitting 4<&0 <file v}
  *)

  val redirect_in  : dup_spec -> (text -> 'o) t -> (text -> 'o) t
  (** Alias for {!(/</)} *)

  val ( />/ )     : ('i -> 'o elem) t -> dup_spec -> ('i -> 'o elem) t
  (** Redirect some outputs from a fitting.  [fitting />/ dups]
   * performs the redirections specifed by [dups] for the extent
   * of [fitting].  For example,
   * [fitting />/ \[ 2 %>& 1 \]] is like
   * {v     % fitting 2>&1 v}
  *)
  val redirect_out : dup_spec -> ('i -> 'o elem) t -> ('i -> 'o elem) t
  (** Alias for {!(/>/)} *)

  val (^>>=)      : ('i -> 'o) t ->
    (Proc.status -> ('i -> 'o) t) ->
    ('i -> 'o) t
  (** Sequence two fittings, with control.
   * Runs its first argument, passes
   * it's exit code to the second argument, and runs the resulting
   * fitting.  The second argument can therefore choose what to do
   * based on the result of the first.
   *
   * The exit code of external processes is the actual exit code as
   * reported by {!Proc.wait}.  The exit code of a shtream is 0 unless
   * the shtream terminates by calling [Shtream.fail_with n], in which
   * case the code in [n].  Or, {!yield} can return an exit code
   * directly.
  *)

  val seq         : ('i -> 'o) t ->
    (Proc.status -> ('i -> 'o) t) ->
    ('i -> 'o) t
  (** Alias for {!(^>>=)} *)

  val (^>>)       : ('i -> 'o) t -> ('i -> 'o) t -> ('i -> 'o) t
  (** Sequence two fittings, ignoring the exit code of the first.
   * [ a ^>> b ] is exactly [ a ^>>= fun _ -> b ].
   * This is like [;] in the shell. *)
  val (&&^)       : ('i -> 'o) t -> ('i -> 'o) t -> ('i -> 'o) t
  (** Sequence two fittings, running the second if the first succeeds.
   * If the first fails, skips the second and propagates the exit
   * code from the first.
   * This is like [&&] in the shell. *)
  val (||^)       : ('i -> 'o) t -> ('i -> 'o) t -> ('i -> 'o) t
  (** Sequence two fittings, running the second if the first fails.
   * If the first succeeds, skips the second and returns an exit code
   * of 0.
   * This is like [||] in the shell. *)

  val (~>>)       : ('i -> 'o) t list -> ('i -> 'o) t
  (** Run a list of fittings in sequence with {!(^>>)}. *)
  val (~&&)       : ('i -> 'o) t list -> ('i -> 'o) t
  (** Run a list of fittings in sequence with {!(&&^)}.
   * Terminates the sequence when any component fails. *)
  val (~||)       : ('i -> 'o) t list -> ('i -> 'o) t
  (** Run a list of fittings in sequence with {!(||^)}.
   * Terminates the sequence when any component succeeds. *)
  val commands    : string list     -> (text -> text) t
  (** Run a list of commands, piping the output of each 
   * into the next. *)

  val yield      : Proc.status -> ('i -> 'o) t
  (** Produces a fitting that returns the given exit code.
   * Has no effect on input and output, but can be used to pass a
   * particular code along in a sequence.
  *)

  val caml       : (unit -> ('i -> 'o) t) -> ('i -> 'o) t
  (** Delay an Ocaml thunk until a fitting is run.  Given a thunk
   * that produces a fitting, {!caml} constructs a new fitting that,
   * when run, forces the thunk and runs the resulting fitting.
   * This allows for Ocaml side-effects at arbitrary points during a
   * fitting.
  *)

  val (^&=) : (text -> 'b elem) t -> (Proc.t -> ('i -> 'o) t) -> ('i -> 'o) t
  (** Run a fitting in the background.  [ bg ^&= fg ] runs [bg]
   * in the background, passed its {!Proc.t} to [fg], and runs
   * the fitting returned by [fg] (in the foreground).
   *
   * Notice that the [bg] must have input type {!text}; it will
   * construct its own input shtream from the standard input. *)
  val par   : (text -> 'b elem) t -> (Proc.t -> ('i -> 'o) t) -> ('i -> 'o) t
  (** Alias for {!(^&=)} *)

  val (^&)  : (text -> 'b elem) t -> ('i -> 'o) t             -> ('i -> 'o) t
  (** Run a fitting in the background, ignore its {!Proc.t}.  This
   * backgrounds its first argument and then continues with its second
   * argument in the foreground. *)

  (** {4 Fitting Runners} *)


  val run_source : (text -> 'o) t      -> 'o shtream
  (** Run a fitting, returning its output as a shtream.  The fitting
   * will take its input from the standard input. *)
  val run_sink   : ('i -> 'o elem) t   -> 'i coshtream
  (** Run a fitting, returning a costhream connected to its input.
   * The fitting will send its output from the standard output. *)
  val run_list   : (text -> 'o) t      -> 'o list
  (** Run a fitting, returning its output as a list.  The fitting
   * will take its input from the standard input. *)

  val run_shtream : ('i -> 'o) t  -> 'i shtream -> 'o shtream
  (** 
   * Transform a fitting into a shtream transformer.
  *)

  val run_in     : ?procref:procref -> (text -> 'o elem) t -> in_channel
  (** Run a fitting, returning its output as an [in_channel].  The fitting
   * will take its input from the standard input.  If [?procref] is
   * provided, the {!Proc.t} of the child process will be stashed.
  *)
  val run_out    : ?procref:procref -> (text -> 'o elem) t -> out_channel
  (** Run a fitting, returning its input as an [out_channel].  The fitting
   * will send its output from the standard output.  If [?procref] is
   * provided, the {!Proc.t} of the child process will be stashed.
  *)

  val run_backquote : ?procref:procref -> (text -> 'o elem) t -> string
  (** Run a fitting, returning its output collected as a string.
   * The exit code of the child process can be retrieved by providing
   * [?procref]. *)

  val run_bg     : (text -> 'o elem) t -> Proc.t
  (** Run a fitting in the background, returning its {!Proc.t}.
   * The fitting will take its input from the standard input and send
   * its output to the standard output. *)
  val run        : (text -> 'o elem) t -> Proc.status
  (** Run a fitting in the foreground, returning its exit status.
   * The fitting will take its input from the standard input and send
   * its output to the standard output. *)

  (** {4 Convenient Conversions} *)

  (** These conversions use the {!elem} conversions provided to the
   * {!Fitting.Make} functor by {!AnyShtream.ELEM}.  The conversion
   * {!AnyShtream.ELEM.string_of} or {!AnyShtream.ELEM.of_string}
   * is completely applied for each of these conversions, so no state
   * (should there be any) is retained in between calls.
  *)

  val string_of_elem  : 'a elem -> string
  (** Convert a shtream element to a string. *)
  val elem_of_string  : string -> text
  (** Convert a string to a shtream element. *)
  val int_of_elem     : 'a elem -> int
  (** Convert a shtream element to an integer. *)
  val elem_of_int     : int -> text
  (** Convert a integer to a shtream element. *)
  val char_of_elem    : 'a elem -> char
  (** Convert a shtream element to a character. *)
  val elem_of_char    : char -> text
  (** Convert a character to a shtream element. *)
  val float_of_elem   : 'a elem -> float
  (** Convert a shtream element to a float. *)
  val elem_of_float   : float -> text
  (** Convert a float to a shtream element. *)
  val bool_of_elem    : 'a elem -> bool
  (** Convert a shtream element to a boolean. *)
  val elem_of_bool    : bool -> text
  (** Convert a boolean to a shtream element. *)
end
