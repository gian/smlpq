(*
 * SQL database interfaces for Standard ML
 * Copyright (C) 2003  Adam Chlipala
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Signature for a full-fledged client interface *)

signature SQL_CLIENT =
sig
    include SQL_DRIVER

    val query : conn -> string -> value list list
    (* Get thr row results of an SQL query over a connection *)

    val oneRow : conn -> string -> value list
    (* Make a query that must return exactly one row *)
    val oneOrNoRows : conn -> string -> value list option
    (* Make a query that may return zero or one row *)

    val app : conn -> (value list -> unit) -> string -> unit
    (* Behaves like List.app over the results of a query *)
    val map : conn -> (value list -> 'a) -> string -> 'a list
    (* Behaves like List.map over the results of a query *)
end

