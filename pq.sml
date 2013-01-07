(*
 * MLton PostgreSQL bindings
 * Copyright (C) 2013 Gian Perrone
 *
 * Adapted from SQL database interfaces for Standard ML
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

structure Pq :> SQL_DRIVER =
struct
    type conn = MLton.Pointer.t
    
    exception Sql of string

    type value = string option
    type result = MLton.Pointer.t

    fun conn connInfo = 
        let
            val cPQconnectdb = _import "PQconnectdb" : string -> conn;
        in
            cPQconnectdb (CString.fromString connInfo)
        end

    fun close c =
        let
            val cPQfinish = _import "PQfinish" : conn -> unit;
        in
            cPQfinish c
        end
   
    fun clear res = 
        let
            val cPQclear = _import "PQclear" : MLton.Pointer.t -> unit;
        in
            cPQclear res
        end

    val cPQexec = _import "PQexec" : conn * string -> result;
    val cPQresultStatus = _import "PQresultStatus" : result -> int;
    val cPQntuples = _import "PQntuples" : result -> int;
    val cPQnfields = _import "PQnfields" : result -> int;
    val cPQgetisnull = _import "PQgetisnull" : result * int * int -> int;
    val cPQgetvalue = _import "PQgetvalue" : result * int * int -> CString.cstring;

    fun makeValue v =
        if v = MLton.Pointer.null then
            NONE
        else
            SOME (CString.toString v)
    
    fun dml conn cmd =
        let
            val q' = CString.fromString cmd
            val res = cPQexec (conn, q')
            fun done () = (clear res; CString.free q')
        in
            case cPQresultStatus res of
                1 => ( done (); "" )
              | n => ( done (); 
                       raise Sql ("Error: " ^ Int.toString n) ) (* FIXME: Produce better errors *)
        end

    fun fold conn f b query =
        let
            val q' = CString.fromString query
            val res = cPQexec (conn, q')
            fun done () = (clear res; CString.free q')
        in
            case cPQresultStatus res of
                2 => 
                    let
                        val nt = cPQntuples res
                        val nf = cPQnfields res

                        fun builder (i, acc) =
                            if i = nt then
                                acc
                            else
                                let
                                    fun build (~1, acc) = acc
                                      | build (j, acc) =
                                            build (j-1,
                                                if cPQgetisnull (res, i, j) <> 0 then
                                                    NONE :: acc
                                                else
                                                    makeValue (cPQgetvalue (res, i, j)) :: acc)
                                in
                                    builder (i+1, f (build (nf-1, []), acc))
                                end
                    in
                        builder (0, b)
                        before done ()
                    end
              | n => ( done (); 
                       raise Sql ("Error: " ^ Int.toString n) )
        end

    type timestamp = Time.time
    exception Format of string

    (* Conversions between SML values and their string representations from SQL queries *)
    fun valueOf v =
    case v of
        NONE => raise Sql "Trying to read NULL value"
      | SOME v => v

    fun isNull s =
    case s of
        NONE => true
      | _ => false

    fun intToSql n =
    if n < 0 then
        "-" ^ Int.toString(~n)
    else
        Int.toString n
    fun intFromSql' "" = 0
      | intFromSql' s =
    (case Int.fromString s of
         NONE => raise Format ("Bad integer: " ^ s)
       | SOME n => n)
    fun intFromSql v = intFromSql' (valueOf v)

    fun stringToSql s =
    let
        fun xch #"'" = "\\'"
          | xch #"\n" = "\\n"
          | xch #"\r" = "\\r"
          | xch c = str c
    in
        foldl (fn (c, s) => s ^ xch c) "'" (String.explode s) ^ "'"
    end
    val stringFromSql = valueOf

    fun realToSql s =
    if s < 0.0 then
        "-" ^ Real.toString(~s)
    else
        Real.toString s
    fun realFromSql' "" = 0.0
      | realFromSql' s =
    (case Real.fromString s of
         NONE => raise Format ("Bad real: " ^ s)
       | SOME r => r)
    fun realFromSql v = realFromSql' (valueOf v)
    fun realToString s = realToSql s

    fun toMonth m =
    let
        open Date
    in
        case m of
        1 => Jan
          | 2 => Feb
          | 3 => Mar
          | 4 => Apr
          | 5 => May
          | 6 => Jun
          | 7 => Jul
          | 8 => Aug
          | 9 => Sep
          | 10 => Oct
          | 11 => Nov
          | 12 => Dec
          | _ => raise Format "Invalid month number"
    end

    fun fromMonth m =
    let
        open Date
    in
        case m of
        Jan => 1
          | Feb => 2
          | Mar => 3
          | Apr => 4
          | May => 5
          | Jun => 6
          | Jul => 7
          | Aug => 8
          | Sep => 9
          | Oct => 10
          | Nov => 11
          | Dec => 12
    end

    fun pad' (s, 0) = s
      | pad' (s, n) = pad' ("0" ^ s, n-1)
    fun pad (n, i) =
    let
        val base = Int.toString n
    in
        pad' (base, Int.max (i - size base, 0))
    end

    fun offsetStr NONE = "+00"
      | offsetStr (SOME n) =
    let
        val n = LargeInt.toInt (Time.toSeconds n) div 3600
    in
        if n < 0 then
        "-" ^ pad (~n, 2)
        else
        "+" ^ pad (n, 2)
    end

    fun timestampToSqlUnquoted t =
    let
        val d = Date.fromTimeLocal t
    in
        pad (Date.year d, 4) ^ "-" ^ pad (fromMonth (Date.month d), 2) ^ "-" ^ pad (Date.day d, 2) ^
        " " ^ pad (Date.hour d, 2) ^ ":" ^ pad (Date.minute d, 2) ^ ":" ^ pad (Date.second d, 2) ^
        ".000000" ^ offsetStr (Date.offset d)
    end
    fun timestampToSql t = "'" ^ timestampToSqlUnquoted t ^ "'"
    fun timestampFromSql' s =
    let
        val tokens = String.tokens (fn ch => ch = #"-" orelse ch = #" " orelse ch = #":"
                         orelse ch = #"." orelse ch = #"+") s
    in
        case tokens of
        [year, mon, day, hour, minute, second, _, offset] =>
        Date.toTime (Date.date {day = valOf (Int.fromString day), hour = valOf (Int.fromString hour), minute = valOf (Int.fromString minute),
                    month = toMonth (valOf (Int.fromString mon)),
                    offset = SOME (Time.fromSeconds (LargeInt.fromInt (valOf (Int.fromString offset) * 3600))),
                    second = valOf (Int.fromString second) div 1000, year = valOf (Int.fromString year)})
          | [year, mon, day, hour, minute, second, _] =>
        Date.toTime (Date.date {day = valOf (Int.fromString day), hour = valOf (Int.fromString hour), minute = valOf (Int.fromString minute),
                    month = toMonth (valOf (Int.fromString mon)),
                    offset = NONE,
                    second = valOf (Int.fromString second), year = valOf (Int.fromString year)})
          | [year, mon, day, hour, minute, second] =>
        Date.toTime (Date.date {day = valOf (Int.fromString day), hour = valOf (Int.fromString hour), minute = valOf (Int.fromString minute),
                    month = toMonth (valOf (Int.fromString mon)),
                    offset = NONE,
                    second = valOf (Int.fromString second) div 1000, year = valOf (Int.fromString year)})
          | _ => raise Format ("Invalid timestamp " ^ s)
    end
    fun timestampFromSql v = timestampFromSql' (valueOf v)
        

    fun boolToSql true = "TRUE"
      | boolToSql false = "FALSE"

    fun boolFromSql' "FALSE" = false
      | boolFromSql' "f" = false
      | boolFromSql' "false" = false
      | boolFromSql' "n" = false
      | boolFromSql' "no" = false
      | boolFromSql' "0" = false
      | boolFromSql' "" = false
      | boolFromSql' _ = true

    fun boolFromSql v = boolFromSql' (valueOf v)
end

structure PgClient = SqlClient(Pq)

