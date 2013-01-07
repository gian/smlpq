
(* Hacky CString module wrapping MLton.Pointer.t *)
structure CString =
struct
    type cstring = MLton.Pointer.t

    (* Convert a cstring to an ML string *)
    fun toString s =
        let
            fun loop (l,i) =
                case MLton.Pointer.getWord8 (s,i) of
                     0w0 => String.implode (rev l)
                   | c => loop ((Byte.byteToChar c) :: l, i + 1)
        in
            loop ([], 0)
        end

    (* Convert an ML string to a null-terminated C string *)
    fun fromString s = s ^ "\000"

    fun free s = ()
end
