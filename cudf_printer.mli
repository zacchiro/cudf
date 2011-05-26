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

(** {6 Pretty print to output channels} *)

val pp_cudf : out_channel -> cudf -> unit
val pp_doc : out_channel -> cudf_doc -> unit
val pp_solution : out_channel -> solution -> unit
val pp_item : out_channel -> cudf_item -> unit
val pp_package : out_channel -> package -> unit
val pp_preamble : out_channel -> preamble -> unit
val pp_request : out_channel -> request -> unit
val pp_packages : out_channel -> package list -> unit
val pp_universe : out_channel -> universe -> unit

