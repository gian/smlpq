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

(* Functor for creating a client from a minimal driver *)

functor SqlClient(Driver : SQL_DRIVER) :> SQL_CLIENT
	    where type conn = Driver.conn =
struct
    open Driver

    fun oneOrNoRows conn query =
	fold conn (fn (row, NONE) => SOME row
		    | (_, SOME _) => raise Sql ("Expected one or zero rows; got multiple for:\n" ^ query)) NONE query

    fun oneRow conn query =
	(case oneOrNoRows conn query of
	     NONE => raise Sql "Expected one row; got none"
	   | SOME row => row)

    fun app conn f q = fold conn (fn (row, ()) => f row) () q
    fun map conn f q = List.rev (fold conn (fn (row, out) => (f row)::out) [] q)

    fun query conn q = List.rev (fold conn op:: [] q)
end

