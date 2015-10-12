module Debug
open mdw
open Xunit
open mdw.DataDefs

(* 
Because VisualStudio cannot detect which Xunit test the cursor is on, 
it's convenient to have a separate file to isolate single tests in order to debug them.
This test should be kept empty in public checkins.
*)

#nowarn "0040"
#nowarn "0058"

open mdw.Packrat
