(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2010  Stefano Zacchiroli <zack@pps.jussieu.fr>        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** Pretty printing of CUDF macro-components (documents, stanzas, ...)

    For pretty printing of micro-components see {!module: Cudf_types_pp}.
*)

open Cudf

(** {6 Pretty print to abstract formatters} *)

val pp_cudf : Format.formatter -> cudf -> unit
val pp_doc : Format.formatter -> cudf_doc -> unit
val pp_solution : Format.formatter -> solution -> unit
val pp_item : Format.formatter -> cudf_item -> unit
val pp_package : Format.formatter -> package -> unit
val pp_preamble : Format.formatter -> preamble -> unit
val pp_request : Format.formatter -> request -> unit
val pp_packages : Format.formatter -> package list -> unit
val pp_universe : Format.formatter -> universe -> unit

(** {6 Pretty print to string}

    Shorthand functions. *)

val string_of_cudf : cudf -> string
val string_of_doc : cudf_doc -> string
val string_of_solution : solution -> string
val string_of_item : cudf_item -> string
val string_of_package : package -> string
val string_of_request : request -> string
val string_of_preamble : preamble -> string
val string_of_packages : package list -> string
val string_of_universe : universe -> string
