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

(** The result of {!AnyShtream.Make} contains all the type-indifferent
 * shtream operations from {!Shtream}. *)

module type AnyShtream = sig

  (** Access to the underlying element type and operations. *)
  module Elem : Elem

  include module type of Shtream_intf

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
  type 'a t constraint 'a = 'b -> 'c

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
