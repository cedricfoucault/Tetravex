		       (* François Thomasset *)
		      (* INRIA - Rocquencourt *)
			      (* 2007 *)
open Tk ;;
let top = openTk () ;;
Wm.title_set top "Canvas 1" ;;
Wm.geometry_set top "320x120"
let c = Canvas.create
    ~width:300
    ~height:100
    ~borderwidth:3
    ~relief:`Raised
    top ;;
ignore (Canvas.create_polygon
	  ~xys:[(40,10);  (60,30);  (60,50);  (60,70);
		(40,90);  (20,70);  (20,50);  (20,30)]
	  ~fill:`Red
	  c);;
ignore (Canvas.create_oval
	  ~x1:100 ~y1:20 ~x2:210 ~y2:80
	  ~fill:`Blue
	  c);;
ignore (Canvas.create_rectangle
  ~x1:6 ~y1:6 ~x2:305 ~y2:105 ~width:1
  c);;
(* ignore (Canvas.create_bitmap
    ~x:250 ~y:30 ~foreground:(`Color "#FF4901")
    ~bitmap:(`File "./gs_l.xbm")
    c);; *)
pack [c];;
let _ = Printexc.print mainLoop ();;