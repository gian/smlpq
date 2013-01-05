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

(* Signature to be implemented to make use of a new client *)

signature SQL_DRIVER =
sig
    type conn
    (* SQL client connection *)

    exception Sql of string
    (* All-purpose exception *)

    type value
    (* One value in a query result *)

    val conn : string -> conn
    (* Connect to a server based on a string of information *)
    val close : conn -> unit
    (* Close a connection *)
    val dml : conn -> string -> string
    (* Execute a DML command over a connection, returning a result message *)
    val fold : conn -> (value list * 'a -> 'a) -> 'a -> string -> 'a
    (* Behaves like List.foldl, applied over the result rows of a query *)

    type timestamp = Time.time
    exception Format of string

    (* Conversions between SML values and their string representations from SQL queries *)
    val isNull : value -> bool
    val intToSql : int -> string
    val intFromSql : value -> int
    val stringToSql : string -> string
    val stringFromSql : value -> string
    val timestampToSql : timestamp -> string
    val timestampToSqlUnquoted : timestamp -> string
    val timestampFromSql : value -> timestamp
    val realToSql : real -> string
    val realFromSql : value -> real
    val boolToSql : bool -> string
    val boolFromSql : value -> bool
end

