(** Record readers and splitters for a variety of file formats.
 * Splitters get raw data from {!Line.raw} and use it to set
 * other line fields.
 *
 * Each adaptor module typically contains three functions:
 * - [adaptor] maps a [LineShtream.initial LineShtream.elem LineShtream.t]
 *   to contain the specified data; if the shtream is being read from a
 *   channel, then it hints to the source shtream the proper record
 *   reader to use.
 * - [reader] is the record reader for the given kind of file.
 * - [splitter] is the field splitter that can be used on a single
 *   record or mapped over a shtream.
 *)

(** A function on lines.  In particular, an
 * [('a -> 'b) splitter] takes ['a Line.t] to ['b Line.t]. *)
type 'ab splitter = 'a Line.t -> 'b Line.t
  constraint 'ab = 'a -> 'b

(** A function on line shtreams.  In particular, an
 * [('a -> 'b) adaptor] maps an ['a Line.t Shtream.t] to a
 * ['b Line.t Shtream.t].  It also provides reader hints; that is,
 * if the input shtream's reader was selected
 * as a default (rather than by the user), then it may adjust the input
 * shtream's reader. *)
type 'ab adaptor = 'a Line.t Shtream.t -> 'b Line.t Shtream.t
  constraint 'ab = 'a -> 'b

(** An adaptor lifted to be used as a fitting. *)
type 'ab fitting_adaptor = unit -> ('a Line.t -> 'b Line.t) Fitting.t
  constraint 'ab = 'a -> 'b

val make_adaptor : ?reader:Reader.t ->
                   ('a -> 'b) splitter ->
                   ('a -> 'b) adaptor
(** Make a shtream adaptor from a reader and a field splitter.  The
 * resulting adaptor can provide reader hints to a shtream from which it
 * reads. *)

(** Conversions for shtream adaptors.  These produce shtream warnings
 * (calling {!Shtream.warn}) if the value cannot be coerced. *)
module Convert : sig
  val convert : (string -> 'a) -> string -> string -> string -> 'a
  (** Create a custom conversion.
   * [Adaptor.Convert.convert conv tyname loc str]
   * applies the conversion [conv] to [str]; if the conversion raises
   * [Failure], then it produces a warning, with [tyname] as the name of
   * the target type and [loc] as the name of the function given in the
   * message. *)

  val to_int    : string -> string -> int
  (** Convert a string to an integer, with shtream warning on failure.
   * [Convert.to_int loc str] returns [str] as an integer or raises a warning
   * attributed to [loc]. *)
  val to_float  : string -> string -> float
  (** Convert a string to a float, with shtream warning on failure.
   * [Convert.to_float loc str] returns [str] as a float or raises a warning
   * attributed to [loc]. *)
end

(** Adaptor modules for flexible handling of delimited text files.
 * This is a thin layer over {!Delimited}, and takes the same
 * {!Delimited.options}.  Results are stored in {!Line.Delim}. *)
module Delim : sig
  val adaptor : ?options:Delimited.options ->
    (<Line| delim: Line.absent; .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) adaptor
  (** Split lines according to the given {!Delimited} options. *)
  val fitting : ?options:Delimited.options ->
    (<Line| delim: Line.absent; .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) fitting_adaptor
  (** Fitting for delimited text files. *)
  val reader : ?options:Delimited.options -> Reader.t
  (** Read records according to the given {!Delimited} options. *)
  val splitter : ?options:Delimited.options ->
    (<Line| delim: Line.absent;                .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) splitter
  (** Split one record according to the given {!Delimited} options. *)

  (** {3 Functorial Interface} *)

  (** Input signature for {!Make}.  Specifies the values necessary for
   * building a specialized delim adaptor. *)
  module type SPEC = sig
    val options : Delimited.options
    (** The options to use for splitting into fields. *)
  end

  (** Output signature for {!Make}. *)
  module type S = sig
    include SPEC
    val adaptor : 
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present >; .. as 'a >) adaptor
    (** Adaptor for a custom delim adaptor. *)
    val fitting :
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present >; .. as 'a >) fitting_adaptor
    (** Fitting for a custom delim adaptor. *)
    val reader : Reader.t
    (** Reader for a custom delim adaptor. *)
    val splitter :
      (<Line| delim: Line.absent; .. as 'a > ->
       <Line| delim: <| options: Line.present >; .. as 'a >) splitter
    (** Splitter for a custom delim adaptor. *)
  end

  module Make(Spec : SPEC) : S
  (** Functor to create custom delim adaptors. *)

  (** Input signature for {!Make_names}.  Specifies the values necessary for
   * building a specialized delim adaptor, including a list
   * of field neams. *)
  module type SPEC_NAMES = sig
    val options : Delimited.options
    (** The options to use for splitting into fields. *)
    val names   : string list
    (** The names of the fields, in order. *)
  end

  (** Output signature for {!Make}. *)
  module type S_NAMES = sig
    include SPEC_NAMES
    val adaptor : 
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present;
                        names: Line.present >; .. as 'a >) adaptor
    (** Adaptor for a custom delim adaptor. *)
    val fitting :
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present;
                        names: Line.present >; .. as 'a >) fitting_adaptor
    (** Fitting for a custom delim adaptor. *)
    val reader : Reader.t
    (** Reader for a custom delim adaptor. *)
    val splitter :
      (<Line| delim: Line.absent; .. as 'a > ->
       <Line| delim: <| options: Line.present;
                        names: Line.present >; .. as 'a >) splitter
    (** Splitter for a custom delim adaptor. *)
  end

  module Make_names(Spec : SPEC_NAMES) : S_NAMES
  (** Functor to create custom delim adaptors with field names. *)
end

(** Adaptor module for simple flat file tables.
 * Handles blank lines, comments, and capping the number of fields. *)
module SimpleFlatFile : sig
  val adaptor : ?comments:string -> ?blanks:bool -> ?max:int -> char ->
    (<Line| delim: Line.absent;                .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) adaptor
  (** Split lines into {!Line.Delim}.  If [comment] is given (default
   * [None]), lines starting with [comment] (ignoring leading white
   * space) are considered comment lines and are ignored by the reader.
   * If [blanks] is true (default [true]), lines consisting entirely of
   * white space are ignored by the record reader.  If [max] is given,
   * then only [max] fields will be produced, and the last field may
   * contain record separatos.  The final [char] argument is the field
   * separator. *)
  val fitting : ?comments:string -> ?blanks:bool -> ?max:int -> char ->
    (<Line| delim: Line.absent;                .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) fitting_adaptor
  (** Fitting for simple flat files. *)
  val reader : ?comments:string -> ?blanks:bool -> Reader.t
  (** Record reader for simple flat files.  See {!adaptor} for options. *)
  val splitter :
    ?max:int -> char ->
    (<Line| delim: Line.absent; .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) splitter
  (** Split a simple flat files record.  See {!adaptor} for options. *)

  (** {3 Functorial Interface} *)

  (** Input signature for {!Make}.  Specifies the values necessary for
   * building a specialized flat file adaptor. *)
  module type SPEC = sig
    val comments : string option
    (** Ignore lines starting with this string. *)
    val blanks   : bool
    (** Ignore blank lines if true. *)
    val max      : int
    (** Split into no more fields than this; unlimited if [0]. *)
    val delim    : char
    (** Split on this delimiter character. *)
  end

  (** Output signature for {!Make}. *)
  module type S = sig
    include SPEC
    val adaptor : 
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present >; .. as 'a >) adaptor
    (** Adaptor for a custom flat file adaptor. *)
    val fitting :
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present >; .. as 'a >) fitting_adaptor
    (** Fitting for a custom flat file adaptor. *)
    val reader : Reader.t
    (** Reader for a custom flat file adaptor. *)
    val splitter :
      (<Line| delim: Line.absent; .. as 'a > ->
       <Line| delim: <| options: Line.present >; .. as 'a >) splitter
    (** Splitter for a custom flat file adaptor. *)
  end

  module Make(Spec : SPEC) : S
  (** Functor to create custom flat file adaptors. *)

  (** Input signature for {!Make_names}.  Specifies the values necessary for
   * building a specialized flat file adaptor, including a list
   * of field neams. *)
  module type SPEC_NAMES = sig
    val comments : string option
    (** Ignore lines starting with this string. *)
    val blanks   : bool
    (** Ignore blank lines if true. *)
    val max      : int
    (** Split into no more fields than this; unlimited if [0]. *)
    val delim    : char
    (** Split on this delimiter character. *)
    val names   : string list
    (** The names of the fields, in order. *)
  end

  (** Output signature for {!Make}. *)
  module type S_NAMES = sig
    include SPEC_NAMES
    val adaptor : 
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present;
                        names: Line.present >; .. as 'a >) adaptor
    (** Adaptor for a custom flat file adaptor. *)
    val fitting :
      (<Line| delim: Line.absent;  .. as 'a > ->
       <Line| delim: <| options: Line.present;
                        names: Line.present >; .. as 'a >) fitting_adaptor
    (** Fitting for a custom flat file adaptor. *)
    val reader : Reader.t
    (** Reader for a custom flat file adaptor. *)
    val splitter :
      (<Line| delim: Line.absent; .. as 'a > ->
       <Line| delim: <| options: Line.present;
                        names: Line.present >; .. as 'a >) splitter
    (** Splitter for a custom flat file adaptor. *)
  end

  module Make_names(Spec : SPEC_NAMES) : S_NAMES
  (** Functor to create custom flat file adaptors with field names. *)
end

(** Adaptor module for key-value property lists.  This file format is
 * essentially pairs of keys and associated values, separated by some
 * character (such as ['='] or [':']).  We discard blank lines and
 * comment lines and join lines ending in a backslash.
 *)
module Key_value : sig
  val adaptor : ?comment:string -> ?delim:char ->
    (<Line| key_value: Line.absent;  .. as 'a > ->
     <Line| key_value: <| >; .. as 'a >) adaptor
  (** Adaptor to parse a key-value file. Lines starting with [?comment]
   * (default ["#"] are considered comments and discarded.  Lines are
   * then split between a key and a value at [?delim] (default ['=']),
   * and leading and trailing white space is discarded. *)
  val fitting : ?comment:string -> ?delim:char ->
    (<Line| key_value: Line.absent;  .. as 'a > ->
     <Line| key_value: <| >; .. as 'a >) fitting_adaptor
  (** Fitting for key-value files. *)
  val reader : ?comment:string -> Reader.t
  (** Read key-value records. *)
  val splitter : ?delim:char ->
    (<Line| key_value: Line.absent; .. as 'a > ->
     <Line| key_value: <| >; .. as 'a >) splitter
  (** Split key-value file records and fill in {!Line.Key_value}. *)

  (** {3 Functorial Interface} *)

  (** Input signature for {!Make}.  Specifies the values necessary for
   * building a specialized key-value adaptor. *)
  module type SPEC = sig
    val delim       : char
    (** Character to separate the key from the value. *)
    val comment     : string
    (** Comment line string. *)
  end

  (** Output signature for {!Make}. *)
  module type S = sig
    include SPEC
    val adaptor : 
      (<Line| key_value: Line.absent;  .. as 'a > ->
       <Line| key_value: <| >; .. as 'a >) adaptor
    (** Adaptor for a custom key-value. *)
    val fitting :
      (<Line| key_value: Line.absent;  .. as 'a > ->
       <Line| key_value: <| >; .. as 'a >) fitting_adaptor
    (** Fitting for a custom key-value. *)
    val reader : Reader.t
    (** Reader for a custom key-value. *)
    val splitter :
      (<Line| key_value: Line.absent; .. as 'a > ->
       <Line| key_value: <| >; .. as 'a >) splitter
    (** Splitter for a custom key-value. *)
  end

  module Make(Spec : SPEC) : S
  (** Functor to create custom key-value adaptors. *)
end

(** Adaptor module for key-value property lists with section headings.
 * This is like {!Key_value}, but we also provide a pattern for
 * recognizing when new sections starts and, optionally, when they end.
 *)
module Key_value_section : sig
  val adaptor : ?comment:string -> ?delim:char ->
                ?end_section:string -> string ->
    (<Line| key_value: Line.absent;  .. as 'a > ->
     <Line| key_value: <| section: Line.present >; .. as 'a >) adaptor
  (** Adaptor to parse a key-value file with sections.
   * [Key_value_section.adaptor pat] creates an adaptor for key-value
   * files, using [pat] to recognize sections.  In particular, [pat]
   * must be Perl-compatible regular expression containing exactly one
   * parenthesized back-reference form, for example, ["\\[(.+)\\]"] or
   * ["Host (.+)"], in which case the matched substring becomes the
   * section heading for subsequent records.  At the beginning of
   * parsing, before [pat] has matched, the current section heading is
   * [""].
   *
   * The optional argument [?end_section] gives a Perl-compatible
   * regular expression that, if it matches a line, indicates the end of
   * the current section.  When this happens, the section reverts to the
   * empty string for subsequent records.  The other optional arguments
   * are as in {!Key_value}.
   *)
  val fitting : ?comment:string -> ?delim:char ->
                ?end_section:string -> string ->
    (<Line| key_value: Line.absent;  .. as 'a > ->
     <Line| key_value: <| section: Line.present >; .. as 'a >) fitting_adaptor
  (** Fitting for key-value files with sections. *)
  val reader : ?comment:string -> Reader.t
  (** Read key-value records. *)
  val splitter : ?delim:char ->
                 ?end_section:string -> string ->
    (<Line| key_value: Line.absent; .. as 'a > ->
     <Line| key_value: <| section: Line.present >; .. as 'a >) splitter
  (** Split key-value records with sections and fill in
   * {!Line.Key_value}, including {!Line.Key_value.section}. *)

  (** {3 Functorial Interface} *)

  (** Input signature for {!Make}.  Specifies the values necessary for
   * building a specialized key-value-section adaptor. *)
  module type SPEC = sig
    val section     : string
    (** Pattern for recognizing sections.  See
     * {!Adaptor.Key_value_section.adaptor} *)
    val delim       : char
    (** Character to separate the key from the value. *)
    val comment     : string
    (** Comment line string. *)
    val end_section : string option
    (** Patter for recognizing the end of a section. *)
  end

  (** Output signature for {!Make}. *)
  module type S = sig
    include SPEC
    val adaptor : 
      (<Line| key_value: Line.absent;  .. as 'a > ->
       <Line| key_value: <| section: Line.present >; .. as 'a >) adaptor
    (** Adaptor for a custom key-value-section. *)
    val fitting :
      (<Line| key_value: Line.absent;  .. as 'a > ->
       <Line| key_value: <| section: Line.present >; .. as 'a >) fitting_adaptor
    (** Fitting for a custom key-value-section. *)
    val reader : Reader.t
    (** Reader for a custom key-value-section. *)
    val new_splitter : unit ->
      (<Line| key_value: Line.absent; .. as 'a > ->
       <Line| key_value: <| section: Line.present >; .. as 'a >) splitter
    (** Splitter constructor for a custom key-value-section.  This
     * function takes [()] as its first argument in order to initialize
     * internal state that keeps track of the current section.  That is,
     * reapplying {!new_splitter} to [()] will yield a splitter that
     * initially is in section [""], and doesn't share its section state
     * with any other splitter.
     *)
  end

  module Make(Spec : SPEC) : S
  (** Functor to create custom key-value-section adaptors. *)
end

(** Adaptor module for comma-separated values files. *)
module Csv : sig
  val adaptor : ?trim_space:bool ->
    (<Line| delim: Line.absent;                .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) adaptor
  (** Adaptor to split a shtream of CSV records. *)
  val fitting : ?trim_space:bool ->
    (<Line| delim: Line.absent;                .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) fitting_adaptor
  (** Fitting for CSV records. *)
  val reader : Reader.t
  (** Read CSV records, including quoting and embedded newlines. *)
  val splitter : ?trim_space:bool ->
    (<Line| delim: Line.absent; .. as 'a > ->
     <Line| delim: <| options: Line.present >; .. as 'a >) splitter
  (** Split a CSV record into fields. *)
end

(** Adaptor module for {b passwd}(5) files. *)
module Passwd : sig
  val adaptor :
    (<Line| passwd: Line.absent;  .. as 'a > ->
     <Line| passwd: Line.present; .. as 'a >) adaptor
  (** Adaptor to parse a passwd file. *)
  val fitting :
    (<Line| passwd: Line.absent;  .. as 'a > ->
     <Line| passwd: Line.present; .. as 'a >) fitting_adaptor
  (** Fitting for passwd files *)
  val reader : Reader.t
  (** Reader for a passwd file. *)
  val splitter :
    (<Line| passwd: Line.absent; .. as 'a > ->
     <Line| passwd: Line.present; .. as 'a >) splitter
  (** Split a passwd file record. *)
end

(** Adaptor module for {b group}(5) files. *)
module Group : sig
  val adaptor :
    (<Line| group: Line.absent;  .. as 'a > ->
     <Line| group: Line.present; .. as 'a >) adaptor
  (** Adaptor to parse a group file. *)
  val fitting :
    (<Line| group: Line.absent;  .. as 'a > ->
     <Line| group: Line.present; .. as 'a >) fitting_adaptor
  (** Fitting for group files *)
  val reader : Reader.t
  (** Reader for a group file. *)
  val splitter :
    (<Line| group: Line.absent; .. as 'a > ->
     <Line| group: Line.present; .. as 'a >) splitter
  (** Split a group file record. *)
end

(** Adaptor module for {b fstab}(5) and {b mtab}(5). *)
module Fstab : sig
  val adaptor :
    (<Line| fstab: Line.absent;  .. as 'a > ->
     <Line| fstab: Line.present; .. as 'a >) adaptor
  (** Adaptor to parse an fstab file. *)
  val fitting :
    (<Line| fstab: Line.absent;  .. as 'a > ->
     <Line| fstab: Line.present; .. as 'a >) fitting_adaptor
  (** Fitting for fstab files. *)
  val reader : Reader.t
  (** Reader for an fstab file. *)
  val splitter :
    (<Line| fstab: Line.absent; .. as 'a > ->
     <Line| fstab: Line.present; .. as 'a >) splitter
  (** Split an fstab file record. *)
end

(** Adaptor module for retrieving file status and mode information. *)
module Stat : sig
  val adaptor : ?dir:string ->
    (<Line| stat: Line.absent;             .. as 'a > ->
     <Line| stat: <| mode: Line.present >; .. as 'a >) adaptor
  (** Fill in the {!Line.Stat} line submodule for a shtream of
   * filenames. *)
  val fitting : ?dir:string ->
    (<Line| stat: Line.absent;             .. as 'a > ->
     <Line| stat: <| mode: Line.present >; .. as 'a >) fitting_adaptor
  (** Fitting for add file metadata. *)
  val reader : Reader.t
  (** Reader for a newline-separated list of filenames. *)
  val splitter : ?dir:string ->
    (<Line| stat: Line.absent; .. as 'a > ->
     <Line| stat: <| mode: Line.present >; .. as 'a >) splitter
  (** Fill in the {!Line.Stat} line submodule for the file named
   * in {!Line.raw}. *)
end

(** Adaptor module for the output of {i ps auxww}.
 * Note that the functions in this module require input lines
 * to have {!Line.seq}, which they use to skip the {b ps} header. *)
module Ps : sig
  val adaptor : ?skip:bool ->
    (<Line| ps: Line.absent;  seq: Line.present; .. as 'a > ->
     <Line| ps: Line.present; seq: Line.present; .. as 'a >) adaptor
  (** Map the output of {b ps}(1) into {!Line.Ps}.
   * The optional argument [?skip] (default [true]) specifies
   * whether the first line is a {b ps} header and should be
   * skipped. *)
  val fitting : ?skip:bool ->
    (<Line| ps: Line.absent;  seq: Line.present; .. as 'a > ->
     <Line| ps: Line.present; seq: Line.present; .. as 'a >) fitting_adaptor
  (** Fitting for parsing {b ps}(1) output. *)
  val reader : Reader.t
  (** Read {b ps}(1) output lines. *)
  val splitter : ?skip:bool ->
    (<Line| ps: Line.absent;  seq: Line.present; .. as 'a > ->
     <Line| ps: Line.present; seq: Line.present; .. as 'a >) splitter
  (** Split a ps record into {!Line.Ps}. *)
end

(** Adaptor module for mailcap files (RFC 1524) *)
module Mailcap : sig
  val adaptor :
    (<Line| mailcap: Line.absent;  .. as 'a > ->
     <Line| mailcap: Line.present; .. as 'a >) adaptor
  (** Adaptor to parse a mailcap file. *)
  val fitting :
    (<Line| mailcap: Line.absent;  .. as 'a > ->
     <Line| mailcap: Line.present; .. as 'a >) fitting_adaptor
  (** Fitting for mailcap files. *)
  val reader : Reader.t
  (** Read mailcap file records. *)
  val splitter :
    (<Line| mailcap: Line.absent; .. as 'a > ->
     <Line| mailcap: Line.present; .. as 'a >) splitter
  (** Split mailcap file records and fill in {!Line.Mailcap}. *)
end

(** Adaptor module for {i /etc/ssh/ssh_config}, etc. *)
module Ssh_config : Key_value_section.S

(** Adaptor module for {i /etc/aliases}, etc. *)
module Etc_aliases : Key_value.S

(** Adaptor module for {i /etc/hosts}, etc. *)
module Etc_hosts : Key_value.S

(** Adaptor module for configuration files in the style of {i php.ini}
 * or configuration files on some proprietary operating systems.
 * These files use = between keys and values, square brackets around
 * section headings, and semicolon to start comment lines. *)
module Ini_file : Key_value_section.S