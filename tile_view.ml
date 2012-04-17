open Tetravex

class tile_view ~puzzle:p ~i:ii ~j:ji ?width:(w0 = 40) ?height:(h0 = 40)
  ?packing:(pack = (fun _ -> ())) ?show:(b=true) () =
  object (self)
    val width = w0
    val height = h0
    val puzzle = p
    val i = ii
    val j = ji
    val drawing_area = GMisc.drawing_area ~width:w0 ~height:h0 ~packing:pack
      ~show:b ()
    (* method width = width
    method height = height
    method puzzle = puzzle
    method i = i
    method j = j *)
    initializer begin  
      let expose (drawing_a:GMisc.drawing_area) _ = begin
        (* let width = self#width and height = self#height in
        let puzzle = self#puzzle and i = self#i and j = self#j in *)
        let tile = Tetravex.Puzzle.get puzzle i j in
        let drawing =
          drawing_a#misc#realize ();
          new GDraw.drawable (drawing_a#misc#window);
        in
        let backing = GDraw.pixmap ~width:width ~height:height () in
        (* background *)
        backing#set_foreground `WHITE;
        backing#rectangle ~x:0 ~y:0 ~width:(width - 1) ~height:(height - 1)
          ~filled:true ();
        (* tile edges *)
        backing#set_foreground `BLACK;
        backing#rectangle ~x:0 ~y:0 ~width:(width - 1) ~height:(height - 1)
          ~filled:false ();
        (* diagonal lines *)
        backing#line ~x:0 ~y:0 ~x:(width - 1) ~y:(height - 1);
        backing#line ~x:(width - 1) ~y:0 ~x:0 ~y:(height - 1);
        (* tile colors *)
        (* right color *)
        let cr = Tetravex.Color.to_string (Tetravex.Tile.right_color tile) in
        let layout = drawing_area#misc#pango_context#create_layout in
        Pango.Layout.set_text layout cr;
        backing#put_layout layout ~x:(3 * width / 4) ~y:(height / 2);
        (* (* top color *)
        let ct = Tetravex.Color.to_string (Tetravex.Tile.top_color tile) in
        backing#string ct ~font:font ~x:(width / 2) ~y:(3 * height / 4);
        (* left color *)
        let cl = Tetravex.Color.to_string (Tetravex.Tile.left_color tile) in
        backing#string cl ~font:font ~x:(width / 4) ~y:(height / 2);
        (* bottom color *)
        let cb = Tetravex.Color.to_string (Tetravex.Tile.bottom_color tile) in
        backing#string cb ~font:font ~x:(width / 2) ~y:(height / 4); *)
        (* draw on the screen *)
        drawing#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 
          ~width:(width - 1) ~height:(height - 1) backing#pixmap;
        false;
      end;
      in
      let _ = 
        drawing_area#event#connect#expose ~callback:(expose drawing_area);
        drawing_area#misc#draw;
      in
      ();
    end
  end;;

let destroy () = GMain.Main.quit ()

let main () = begin
  (* Create a new window and sets the border width of the window. *)
  let window = GWindow.window ~width:100 ~height:100 () in

  (* Here we connect the "destroy" event to a signal handler.  
   * This event occurs when we call window#destroy method
   * or if we return [false] in the "delete_event" callback. *)
  window#connect#destroy ~callback:destroy;
  
  let p = Tetravex.Puzzle.dumb_generate 4 4 10 in
  let t = new tile_view ~puzzle:p ~i:2 ~j:2 ~packing:window#add in

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

