(* file: hello.ml *)

(* This is a callback function. *)
let hello () =
  print_endline "Hello World";
  flush stdout
  
let destroy () = GMain.Main.quit ()

let height = 20;;
let width = 20;;

let main () = begin
  (* Create a new window and sets the border width of the window. *)
  let window = GWindow.window ~width:100 ~height:100 () in
  
  (* Here we connect the "destroy" event to a signal handler.  
   * This event occurs when we call window#destroy method
   * or if we return [false] in the "delete_event" callback. *)
  window#connect#destroy ~callback:destroy;
  
  let vbox = GPack.vbox ~packing:window#add () in
  let hbox = GPack.hbox ~packing:vbox#add () in

  (* Creates a new button with the label "Hello World".
   * and packs the button into the window (a gtk container). *)
  let button_new_game = 
    GButton.button ~label:"New Game" ~packing:hbox#add ()
  and button_solve =
    GButton.button ~label:"Solve" ~packing:hbox#add ()
  and button_open =
    GButton.button ~label:"Open" ~packing:hbox#add ()
  and button_save =
    GButton.button ~label:"Save" ~packing:hbox#add ()
  in
  
  let tile =
    GMisc.drawing_area ~width:width ~height:height 
      ~packing:vbox#add ~show:true ()
  in
  let tile_graphics = GDraw.pixmap ~width:width ~height:height () in
  tile_graphics#set_foreground `WHITE;
  tile_graphics#rectangle ~x:0 ~y:0 ~width:width ~height:height 
    ~filled:true ();
  tile_graphics#set_foreground `BLACK;
  tile_graphics#rectangle ~x:0 ~y:0 ~width:(width - 1) ~height:(height - 1) 
    ~filled:false ();
  tile_graphics#line ~x:0 ~y:0 ~x:(width - 1) ~y:(height - 1);
  tile_graphics#line ~x:(width - 1) ~y:0 ~x:0 ~y:(height - 1);
  (* tile_graphics#lines [(0, 0); (width - 1, height - 1); 
    (width - 1, 0); (0, height - 1)]; *)
  let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
    let area = GdkEvent.Expose.area ev in
    let x = Gdk.Rectangle.x area in
    let y = Gdk.Rectangle.y area in
    let width = Gdk.Rectangle.width area in
    let height = Gdk.Rectangle.width area in
    let drawing =
      drawing_area#misc#realize ();
      new GDraw.drawable (drawing_area#misc#window);
    in
    drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
    false;
  in
  tile#event#connect#expose ~callback:(expose tile (ref tile_graphics));
  (* tile#misc#draw None; *)
  (* tile_graphics#string "0" ~font:(Gdk.Font.load "Sans 10")
    ~x:2 ~y:2; *)
  (* tile_graphics#polygon ~filled:true [(0, 0); (10, 10); (0, 20)] *)

  (* When the button receives the "clicked" signal, it will call the
   * function hello().  The hello() function is defined above. *)
  button_new_game#connect#clicked ~callback:hello;
  
  button_solve#connect#clicked ~callback:hello;
  
  button_open#connect#clicked ~callback:hello;
  
  button_save#connect#clicked ~callback:hello;

  (* This will cause the window to be destroyed by calling
   * window#destroy () when "clicked".  Again, the destroy
   * signal could come from here, or the window manager. *)
  (* button#connect#clicked ~callback:window#destroy; *)

  (* The final step is to display the window. *)
  window#show ();

  (* All GTK applications must have a GMain.Main.main (). Control ends here
   * and waits for an event to occur (like a key press or
   * mouse event). *)
  GMain.Main.main ();
end;;

let _ = main ()
