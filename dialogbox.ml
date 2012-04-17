		       (* François Thomasset *)
		      (* INRIA - Rocquencourt *)
			      (* 2007 *)
open Tk ;;
let top = openTk () ;;
Wm.title_set top "Dialog 2" ;;
Wm.geometry_set top "200x100" ;;
(* let bq = Button.create ~text:"Quit"
    ~command:(fun () ->
      let d = Dialog.create
    ~parent:top
    ~title:"Dialog"
    ~message:"Do you really want to quit?"
    ~buttons:["Yes";"No"]
    ~default:1
    ()
      in
      if d=0 then (print_endline "Bye"; flush stdout; closeTk ())
      else (print_endline "OK"; flush stdout))
    top ;;
pack [bq];; *)

let file_select = Tk.getOpenFile ~parent:top ()
in begin
  print_endline file_select;
  flush stdout;
  closeTk();
end;;
let _ = Printexc.print mainLoop ();;

