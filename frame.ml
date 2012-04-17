open Tk ;;

let get width height = begin
  let top = openTk () in
  Wm.title_set top "Tetravex";
  Wm.geometry_set top (Printf.sprintf "%dx%d+200+200" width height);
  let border_width = 2 in
  let header = Frame.create ~relief:`Sunken ~borderwidth:border_width top in
  let b_new = Button.create ~text:"New game" header
  and b_sol = Button.create ~text:"Get Solution" header
  and b_open = Button.create ~text:"Open" header
  and b_save = Button.create ~text:"Save" header in
  let buttons_width =
    Winfo.reqwidth b_new + Winfo.reqwidth b_sol +
    Winfo.reqwidth b_open + Winfo.reqwidth b_save
  in
  (* calculate the horizontal padding between each button *)
  let padx = (width - border_width * 2 - buttons_width) / (8) in
  pack [b_new; b_sol; b_open; b_save] ~side:`Left ~padx:padx;
  pack [header] ~side:`Top ~fill:`X;
  top;
end;;

let new_game_button action header =
  Button.create ~text:"New Game" ~command:action header
  
let get_solution_button action header =
  Button.create ~text:"Get Solution" ~command:action header

let open_button open_callback header =
  let get_file () = Tk.getOpenFile ~parent:header () in
  Button.create ~text:"Open" ~command:(fun () ->
      open_callback (get_file ());
    ) header
  
let save_button save_callback header =
  let get_file () = Tk.getSaveFile ~parent:header () in
  Button.create ~text:"Save" ~command:(fun () ->
      save_callback (get_file ());
    ) header

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

(* let file_select = Tk.getOpenFile ~parent:top ()
in begin
  print_endline file_select;
  flush stdout;
  closeTk();
end;; *)
let _ = begin
  let _ = get 400 400 in
  Printexc.print mainLoop ();
end;;

