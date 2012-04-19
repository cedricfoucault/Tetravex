open Tk ;;
open Tetravex;;

module Window = struct
  type t = Widget.toplevel Widget.widget
  let create width height = begin
    let top = openTk () in
    Wm.title_set top "Tetravex";
    Wm.geometry_set top (Printf.sprintf "%dx%d+200+200" width height);
    top;
  end
  let resize window width height = begin
    Wm.geometry_set window (Printf.sprintf "%dx%d+200+200" width height);
  end
  let close () = closeTk ();
end

module Menu = struct
  type t = Widget.frame Widget.widget
  let border_width = 2
  let init parent = 
    Frame.create ~relief:`Raised ~borderwidth:border_width parent
  let newgame_button newgame_callback menu =
    Button.create ~text:"New Game" ~command:newgame_callback menu
  let solve_button solve_callback menu =
    Button.create ~text:"Get Solution" ~command:solve_callback menu
  let open_button open_callback menu =
    let get_file () = Tk.getOpenFile ~initialdir:"." ~parent:menu () in
    Button.create ~text:"Open" ~command:(fun () ->
        open_callback (get_file ());
      ) menu
  let save_button save_callback menu =
    let get_file () = Tk.getSaveFile ~initialdir:"." ~parent:menu () in
    Button.create ~text:"Save" ~command:(fun () ->
        save_callback (get_file ());
      ) menu

  let create ~newgamecallback:ngc ~solvecallback:sc ~opencallback:oc
    ~savecallback:sac ~width:width ~rows:m ~columns:n parent = begin
    let menu = init parent in
    let button_frame = Frame.create (* ~relief:`Flat *) menu
    and entry_frame = Frame.create (* ~relief`Flat *) menu in
    let rows_v = Textvariable.create ()
    and columns_v = Textvariable.create () in
    let newgame_callback () = begin
      try
        let m = int_of_string (Textvariable.get rows_v)
        and n = int_of_string (Textvariable.get columns_v) in
        if (m > 1) && (m < 10) && (n > 1) && (n < 10) then begin
          ngc m n;
        end else begin
          Printf.fprintf stderr 
            "The number of rows and of columns must be in [2..9]\n";
          flush stderr;
        end
      with
      | _ -> begin
        Printf.fprintf stderr "%s" ("Hey, I didn't get the numbers you typed"
        ^ " are you trying to fool me?!\n");
        flush stderr;
      end
    end in
    let buttons = [
      newgame_button newgame_callback button_frame;
      solve_button sc button_frame;
      open_button oc button_frame;
      save_button sac button_frame;
    ] in
    let sum_width acc button = acc + (Winfo.reqwidth button) in
    let buttons_width = List.fold_left sum_width 0 buttons in
    let padx =  
      (width - buttons_width - border_width * 2) / ((List.length buttons) * 2)
    in
    let rows_entry = Entry.create ~width:1 ~relief:`Sunken 
      ~font:"Helvetica" ~textvariable:rows_v entry_frame
    and columns_entry = Entry.create ~width:1 ~relief:`Sunken 
        ~font:"Helvetica" ~textvariable:columns_v entry_frame in
    let times_label = Label.create ~font:"Helvetica" ~text:"×" entry_frame in
    Textvariable.set rows_v (string_of_int m);
    Textvariable.set columns_v (string_of_int n);
    pack buttons ~side:`Left ~padx:padx;
    pack [coe rows_entry; coe times_label; coe columns_entry] 
      ~side:`Left  ~fill:`X;
    pack [button_frame] ~side:`Top;
    pack [entry_frame] ~anchor:`W ~side:`Top ~padx:padx;
    menu;
  end;;
end


module TileView = struct
  type t = Widget.canvas Widget.widget
  let colors = (Array.make 10 `Black);;
  let _ = begin
    colors.(1) <- `Color "#d2691e"; (* brown *)
    colors.(2) <- `Color "#ff0000"; (* red *)
    colors.(3) <- `Color "#ffa500"; (* orange *)
    colors.(4) <- `Color "#8a2be2"; (* violet *)
    colors.(5) <- `Color "#0000ff"; (* blue *)
    colors.(6) <- `Color "#00ffff"; (* cyan *)
    colors.(7) <- `Color "#7fff00"; (* green *)
    colors.(8) <- `Color "#ffff00"; (* yellow *)
    colors.(9) <- `White;
  end;;
  let face_color i = if i < 10 then colors.(i) else `White
  let string_color i = if i < 6 then `White else `Black
  
  let create ~model:t ?width:(width=40) ?height:(height=40) parent =
    if Tetravex.Tile.is_empty t then 
      Canvas.create ~width:width ~height:height 
        ~relief:`Sunken ~borderwidth:2 ~background:(`Color "#eeeeee") parent
    else begin
    let tile =
      Canvas.create ~width:width ~height:height
          ~relief:`Raised ~borderwidth:2 parent in
    let offset = 5 in
    let font = "Helvetica" in
    let cr = Tetravex.Color.to_int (Tetravex.Tile.right_color t)
    and ct = Tetravex.Color.to_int (Tetravex.Tile.top_color t)
    and cl = Tetravex.Color.to_int (Tetravex.Tile.left_color t)
    and cb = Tetravex.Color.to_int (Tetravex.Tile.bottom_color t)
    in
    let _ = (Canvas.create_polygon ~xys:[
    	  (width + offset, offset);
    	  ((width + offset) / 2, (height + offset) / 2);
    	  (width + offset, height + offset);
    	  ] ~fill:(face_color cr) tile);
    and _ =(Canvas.create_text 
      ~text:(string_of_int cr)  ~fill:(string_color cr)
      ~x:(width * 7 / 8 + offset) ~y:(height / 2 + offset)
      ~font:font ~anchor:`Center tile);
    (* top face *)
    and _ = Canvas.create_polygon ~xys:[
    	  (offset, offset);
    	  (width / 2 + offset, height / 2 + offset);
    	  (width + offset, offset);
    	  ] ~fill:(face_color ct) tile;
    and _ = Canvas.create_text 
      ~text:(string_of_int ct) ~fill:(string_color ct)
      ~x:(width / 2 + offset) ~y:(height * 1 / 8 + offset + 3)
      ~font:font ~anchor:`Center tile;
    (* left face *)
    and _ = Canvas.create_polygon ~xys:[
    	  (offset, offset);
    	  (width / 2 + offset, height / 2 + offset);
    	  (offset, height + offset);
    	  ] ~fill:(face_color cl) tile;
    and _ = Canvas.create_text
      ~text:(string_of_int cl) ~fill:(string_color cl)
      ~x:(width * 1 / 8 + offset) ~y:(height / 2 + offset)
      ~font:font ~anchor:`Center tile;
    (* bottom face *)
    and _ = Canvas.create_polygon ~xys:[
    	  (offset, height + offset);
    	  (width / 2 + offset, height / 2 + offset);
    	  (width + offset, height + offset);
    	  ] ~fill:(face_color cb) tile;
    and _ = Canvas.create_text 
      ~text:(string_of_int cb) ~fill:(string_color cb)
      ~x:(width / 2 + offset) ~y:(height * 7 / 8 + offset - 3)
      ~font:font ~anchor:`Center tile
    in
    tile;
  end;;
end

exception No_Tile

class puzzle_grid ~model:puzzle_model ?width:(w0=200) ?height:(h0=200)
parent =
  object (self)
    val width = w0
    val height = h0
    val m = Tetravex.Puzzle.height puzzle_model
    val n = Tetravex.Puzzle.width puzzle_model
    val model = puzzle_model
    val grid = Frame.create ~relief:`Groove ~borderwidth:2 parent
    val mutable tiles = begin
      Array.make_matrix 0 0 (TileView.create 
        ~model:(Tetravex.Tile.empty ()) ~width:(0) ~height:(0)
        parent);
    end
    val mutable drag_start_function = 
      (fun ~row:_ -> fun ~col:_ -> fun ~model:_ -> fun ~view:_ -> fun _ -> ())
    val mutable drag_move_function = 
      (fun ~model:_ -> fun ~view:_ -> fun _ -> ())
    val mutable drag_stop_function = 
      (fun ~model:_ -> fun ~view:_ -> fun _ -> ())
    
    method get_frame = grid
    
    method bind_drag_start f = drag_start_function <- f;
    method bind_drag_move f = drag_move_function <- f;
    method bind_drag_stop f = drag_stop_function <- f;
    
    method locate_tile x y = begin
      let tile_width = width
      and tile_height = height in
      let j = (x - tile_width / 2) / (tile_width)
      and i = (y - tile_height / 2) / (tile_height) in
      if (i >= 0) && (i < m) &&(j >= 0) && (j < n) then
        (i, j)
      else raise No_Tile
    end

    method private update_bindings () = begin
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          let tile_model = Tetravex.Puzzle.get model i j
          and tile_view = tiles.(i).(j) in
          let drag_start ev = drag_start_function
            ~row:i ~col:j
            ~model:tile_model
            ~view:tile_view ev
          and drag_move ev = drag_move_function
            ~model:tile_model
            ~view:tile_view ev
          and drag_stop ev = drag_stop_function
            ~model:tile_model
            ~view:tile_view ev
          in
          Tk.bind ~events:[`ButtonPressDetail(1)] ~extend:false
            ~fields:[`MouseX; `MouseY] ~action:drag_start tile_view;
          Tk.bind ~events:[`Motion] ~extend:false
            ~fields:[`MouseX; `MouseY] ~action:drag_move tile_view;
          Tk.bind ~events:[`ButtonReleaseDetail(1)] ~extend:false
            ~fields:[`MouseX; `MouseY] ~action:drag_stop tile_view;
        done
      done;
    end
    
    method private update_tiles () = begin
       Array.iter (fun line ->
         Array.iter (fun tile_view ->
           Grid.forget [tile_view];
           Widget.remove tile_view;
         ) line;
       ) tiles;
       tiles <- 
         Array.init m (fun i ->
           Array.init n (fun j -> 
             let tile_model = Tetravex.Puzzle.get puzzle_model i j in
             TileView.create ~model:tile_model
               ~width:(width) ~height:(height) grid;
           )
         );
     end
    
    method update () = begin
      self#update_tiles ();
      self#update_bindings();
      (* let tiles = self#tiles in *)
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          Tk.grid ~column:j ~row:i ~padx:0 ~pady:0 [tiles.(i).(j)];
        done
      done;
    end
    
    method destroy () = begin
      Array.iter (fun line ->
        Array.iter (fun tile_view ->
          Grid.forget [tile_view];
          Widget.remove tile_view;
        ) line;
      ) tiles;
      Pack.forget [grid];
      Widget.remove grid;
      tiles <- Array.make_matrix 0 0 (TileView.create 
        ~model:(Tetravex.Tile.empty ()) ~width:(0) ~height:(0)
        parent);
    end     
    
    initializer begin
      (* self#update (); *)
      (* pack [self#get_frame] ~side:`Left ~padx:50; *)
    end
  end

let glob_id = ref 0;;
type placed_tile = {
  id : int;
  model : Tetravex.Tile.t;
  view : TileView.t;
  mutable x : int;
  mutable y : int;
}

class tile_set ~model:puzzle_model ?width:(w0=60) ?height:(h0=60)
  ?x:(x0=300) ?y:(y0=200) parent =
  let rec remove_aux id = begin function
  | hd :: tl -> if (hd.id = id) then begin
      Place.forget hd.view;
      Widget.remove hd.view;
      tl;
    end else hd :: remove_aux id tl
  | [] -> [];
  end in
  let rec update_pos_aux id x y = begin function
  | hd :: tl -> if (hd.id = id) then begin
      hd.x <- x;
      hd.y <- y;
    end else update_pos_aux id x y tl;
  | [] -> ();
  end in
  object (self)
    val mutable set = []
    (* model x y  *)
    method add ~model:tile_model ~x:xi ~y:yi () = begin
      let current_id = !glob_id in
      set <- {
        id = current_id;
        model = tile_model;
        view = 
          TileView.create ~model:tile_model ~width:w0 ~height:h0 parent;
        x = xi;
        y = yi;
      } :: set;
      incr glob_id;
      current_id;
    end
    method place ~model:tile_model ~view:tile_view ~x:xi ~y:yi () = begin
      let current_id = !glob_id in 
      set <- {
        id = current_id;
        model = tile_model;
        view = tile_view;
        x = xi;
        y = yi;
      } :: set;
      incr glob_id;
      current_id;
    end
    method remove ~id:id () = set <- (remove_aux id set);
    
    val mutable drag_start_function =
      (fun ~id:_ -> fun ~model:_ -> fun ~view:_ -> fun _ -> ())
    val mutable drag_move_function =
      (fun ~id:_ -> fun ~model:_ -> fun ~view:_ -> fun _ -> ())
    val mutable drag_stop_function =
      (fun ~id:_ -> fun ~model:_ -> fun ~view:_ -> fun _ -> ())
      
    method bind_drag_start f = drag_start_function <- f
    method bind_drag_move f = drag_move_function <- f
    method bind_drag_stop f = drag_stop_function <- f
    
    method private update_bindings () = begin
      List.iter (fun placed_tile ->
        let drag_start ev = drag_start_function 
          ~id:(placed_tile.id)
          ~model:(placed_tile.model)
          ~view:(placed_tile.view) ev
        and drag_move ev = drag_move_function 
          ~id:(placed_tile.id)
          ~model:(placed_tile.model)
          ~view:(placed_tile.view) ev
        and drag_stop ev = drag_stop_function 
          ~id:(placed_tile.id)
          ~model:(placed_tile.model)
          ~view:(placed_tile.view) ev
        in
        Tk.bind ~events:[`ButtonPressDetail(1)] ~extend:false
          ~fields:[`MouseX; `MouseY]
          ~action:(drag_start) (placed_tile.view);
        Tk.bind ~events:[`Motion] ~extend:false
          ~fields:[`MouseX; `MouseY]
          ~action:(drag_move) (placed_tile.view);
        Tk.bind ~events:[`ButtonReleaseDetail(1)] ~extend:false
          ~fields:[`MouseX; `MouseY]
          ~action:(drag_stop) (placed_tile.view);
      ) set;
    end
    
    method fire_drag_start ~id:id ev = begin
      let rec find_by_id id = function
      | hd :: tl -> if hd.id = id then hd else find_by_id id tl
      | [] -> failwith "not found (can't happen)"
      in
      let placed_tile = find_by_id id set in
      drag_start_function ~id:placed_tile.id ~model:placed_tile.model
        ~view:placed_tile.view ev;
    end

    method update_pos ~id:id ~x:x ~y:y () = begin
      update_pos_aux id x y set;
      self#update ();
    end
    
    method destroy () = begin
      List.iter (fun placed_tile -> begin
        Place.forget placed_tile.view;
        Widget.remove placed_tile.view;
      end) set;
      set <- [];
    end
    
    method update () = begin
      List.iter (fun placed_tile ->
        Tk.place ~x:(placed_tile.x) ~y:(placed_tile.y) placed_tile.view;
      ) set;
      self#update_bindings ();
    end
    
    initializer begin
      let make_set_from_model p_model =
        let m = Tetravex.Puzzle.height p_model
        and n = Tetravex.Puzzle.width p_model in
        let padx = 5 and pady = 5 in
        for i = 0 to m - 1 do
          for j = 0 to n - 1 do
            let tile_model = Tetravex.Puzzle.get p_model i j in
            ignore (self#add ~model:tile_model ~x:(x0 + j * (w0 + padx))
              ~y:(y0+ i * (h0 + pady)) ());
          done
        done
      in
      make_set_from_model puzzle_model;
      (* self#update (); *)
    end
  end

