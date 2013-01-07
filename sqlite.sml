(*
 * MLton SQLite bindings
 * Copyright (C) 2013 Filip Sieczkowski
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

structure SQLite :> SQL_DRIVER =
struct
    type conn = MLton.Pointer.t
    
    exception Sql of string

    type byte = Word8.word
    type value = string option
    type result = MLton.Pointer.t

    fun sqlite_errstr n =
	let val errstr = _import "sqlite3_errstr" : int -> result;
	    val cstr = errstr n
	in CString.toString cstr
	end

    fun close c =
        let
	    val sqlite_close = _import "sqlite3_close" : conn -> int;
	    val retc = sqlite_close c
        in
	    if retc = 0 then () else raise Sql (sqlite_errstr retc)
        end

    fun conn connInfo = 
        let
            val sqlite_open = _import "sqlite3_open" :
			       string * conn ref -> int;
	    val dbref = ref MLton.Pointer.null
	    val retc  = sqlite_open (connInfo, dbref)
        in
            if retc = 0 then !dbref
	    else (raise Sql (sqlite_errstr retc) before close (!dbref))
        end

    type query = MLton.Pointer.t
    val sqlite_prep = _import "sqlite3_prepare" :
		      conn * string * int * query ref * result ref -> int;
    val sqlite_step = _import "sqlite3_step" : query -> int;
    val sqlite_finalize = _import "sqlite3_finalize" : query -> int;

    val sqlite_colcnt = _import "sqlite3_column_count" : query -> int;
    val sqlite_colT = _import "sqlite3_column_type" : query * int -> int;
    val sqlite_nbytes = _import "sqlite3_column_bytes" : query * int -> int;

    (*val sqlite_VBlob = _import "sqlite3_column_blob" : query * int -> result;
    val sqlite_VInt  = _import "sqlite3_column_int"  : query * int -> int;
    val sqlite_VReal = _import "sqlite3_column_double" : query * int -> real;*)
    val sqlite_VString = _import "sqlite3_column_text" : query * int -> result;

    (* does not work, since function pointers used
    val sqlite_exec = _import "sqlite3_exec" :
		      conn * string *
		      (MLton.Pointer.t * int * MLton.Pointer.t * MLton.Pointer.t -> int)
		      * MLton.Pointer.t * string ref -> int;*)

    fun dml conn cmd =
	let val q = CString.fromString cmd
	    val cqr = ref MLton.Pointer.null
	    val rstr = ref MLton.Pointer.null
	    val prep = sqlite_prep (conn, q, size cmd, cqr, rstr)
	    val _ = if prep = 0 then ()
		    else (sqlite_finalize (!cqr); raise Sql (sqlite_errstr prep))
	    val run = sqlite_step (!cqr)
	    val _ = if run = 101 then ()
		    else (sqlite_finalize (!cqr); 
			  if run = 100 then raise Sql "DML call returned a row"
			  else raise Sql (sqlite_errstr run))
	    val fin = sqlite_finalize (!cqr)
	in
	    if fin = 0 then "" else raise Sql (sqlite_errstr fin)
	end

    fun fold conn (f : value list * 'a -> 'a) sv cmd =
	let val q = CString.fromString cmd
	    val cqr = ref MLton.Pointer.null
	    val rstr = ref MLton.Pointer.null
	    val prep = sqlite_prep (conn, q, size cmd, cqr, rstr)
	    val _ = if prep = 0 then ()
		    else (sqlite_finalize (!cqr); raise Sql (sqlite_errstr prep))
	    val ncols = sqlite_colcnt (!cqr)
	    fun getRest n acc =
		if n = ncols then acc
		else
		    let val typ = sqlite_colT (!cqr, n)
			val v = if typ = 5 then NONE
				else if typ >= 1 andalso typ < 5 then
				    (SOME o CString.toString o sqlite_VString) (!cqr, n)
				else raise Sql "Undefined type returned (this really should not happen, something is probably wrong with sqlite)"
		    in getRest (n + 1) (v :: acc)
		    end
	    fun getVals () = rev (getRest 0 [])
	    fun runFold st =
		let val run = sqlite_step (!cqr)
		    val done = if run = 101 then true
			       else if run = 100 then false
			       else (sqlite_finalize (!cqr); raise Sql (sqlite_errstr run))
		in
		    if done then st else runFold (f (getVals (), st))
		end
	    val res = runFold sv
	    val fin = sqlite_finalize (!cqr)
	in
	    if fin = 0 then res else raise Sql (sqlite_errstr fin)
	end

    type timestamp = Time.time
    exception Format of string

    (* Conversions between SML values and their string representations from SQL queries *)

    fun valueOf v =
    case v of
        NONE => raise Sql "Trying to read NULL value"
      | SOME v => v

    fun isNull v =
	case v of
            NONE => true
	  | _ => false

(*    fun blobToString b = (String.implode o rev) (foldl (fn (b, xs) => Byte.byteToChar b :: xs) [] b)*)

    fun intToSql n =
	if n < 0 then
            "-" ^ Int.toString(~n)
	else
            Int.toString n
    fun intFromSqlS "" = 0
      | intFromSqlS s =
	(case Int.fromString s of
             NONE => raise Format ("Bad integer: " ^ s)
	   | SOME n => n)
    val intFromSql = intFromSqlS o valueOf

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
    val realFromSql = realFromSql' o valueOf
    fun realToString s = realToSql s

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
    val timestampFromSql = timestampFromSql' o valueOf


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

    val boolFromSql = boolFromSql' o valueOf

end

structure SQLiteClient = SqlClient(SQLite)
