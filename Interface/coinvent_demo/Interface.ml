let path_home = Unix.getenv "HOME"
let path = path_home^"/coinvent_demo/"
let locale = GtkMain.Main.init ()
let lang_file = path^"casl.lang"
let font_name = "Monospace 10"
let win = GWindow.window ~allow_grow:true ~allow_shrink:true ~border_width:10 ~width:1000 ~height:800 ~title:"CoInvent" ()
let vbox1 = GPack.vbox ~packing:win#add ()
let scrolled_window = ref (GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()) 

let clipboard = GData.clipboard Gdk.Atom.clipboard
let sp1 = ref ""
let sp2 = ref ""
let music = ref (GMenu.radio_menu_item ())
let musicprog = ref (GMenu.radio_menu_item ())
let maths = ref (GMenu.radio_menu_item ())
let other = ref (GMenu.radio_menu_item ())
let amalgams = ref (GMenu.radio_menu_item ())
let hdtp = ref (GMenu.radio_menu_item ())
let cached = ref (GMenu.check_menu_item ())

let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  ignore(GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ());

  (* Create a label for the button and pack into box *)
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()

let alert s  = 
  let w = GWindow.dialog ~title:"Coinvent Warning" ~height:100 ~width:300 ~modal:true () in
  let but = GButton.button  ~packing:w#action_area#add () in 
  let _label2 = (xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"OK" ~packing:but#add ()) in
  let _label = GMisc.label ~text:s ~packing:w#vbox#add () in
  let _ = but#connect#clicked ~callback:(w#destroy) in
  w#show ()


let strip_spaces buf = 
  String.trim buf

let r_open_in s = 
  let ic = open_in s in
  let size = in_channel_length ic in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let get_lines2 filename =
  let chan = open_in filename in
  let lines = ref [] in
  try
    while true; do
      let l = input_line chan in
      lines := (l,path^"cached/"^l)::!lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines



let get_lines filename =
  let chan = open_in filename in
  let lines = ref [] in
  try
    while true; do
      let l = input_line chan in
      lines := (l,"/home/ewen/coinvent/Amalgamation/"^l)::!lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let nothing () = ()


let run2dca () = 
  ignore(Unix.system(path^"2dca.jar&"));()
(* PARSE FOR MUSIC OR MATHEMATICS HERE - CALL PDF *)

let vpain_all = GPack.paned `VERTICAL ~packing:vbox1#add ()
let vblt = GPack.vbox ~packing:vpain_all#add1 ()
let menu_bar = GMenu.menu_bar ~packing:vblt#pack ()
let frame_main = GBin.frame ~packing:vblt#pack () 

let bbox_main = GPack.button_box `HORIZONTAL ~border_width:15 ~layout:`SPREAD
    ~height:50 ~child_height:20 ~child_width:50 ~spacing:20 ~packing:frame_main#add () 

let button_open1 = GButton.button ~packing:bbox_main#add  () 
let button_open2 = GButton.button ~packing:bbox_main#add  ()
let button_blend = GButton.button ~packing:bbox_main#add  ()
let button_render = GButton.button ~packing:bbox_main#add  ()  
let button_quit = GButton.button ~packing:bbox_main#add  ()  

let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Openl" ~packing:button_open1#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Openr" ~packing:button_open2#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Blend" ~packing:button_blend#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Render" ~packing:button_render#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Quit" ~packing:button_quit#add ()

let file_item = GMenu.menu_item ~label:"File" ~packing:menu_bar#append()
let options_item = GMenu.menu_item ~label:"Options" ~packing:menu_bar#append()
let theory_item = GMenu.menu_item ~label:"Theory" ~packing:menu_bar#append()
let action_item = GMenu.menu_item ~label:"Action" ~packing:menu_bar#append()

let vpain_lem_thm = GPack.paned `HORIZONTAL ~packing:vblt#add ()

let scrolled_win1 = (GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ~packing:vpain_lem_thm#add1
     ())

let source_view1 = ref
  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:scrolled_win1#add 
    ())

let _ = ignore(!source_view1#source_buffer#connect#changed ~callback:(fun () -> !source_view1#source_buffer#add_selection_clipboard clipboard))
 
let l = GSourceView2.source_language_manager ~default:true

let lang = l#guess_language ~content_type:"text/casl" () 

let _ = ignore(!source_view1#source_buffer#set_language lang)

let _ = ignore(!source_view1#source_buffer#set_language lang)

let scrolled_win2 = (GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ~packing:vpain_lem_thm#add2
     ())

let source_view2 = ref
  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:scrolled_win2#add 
    ())

let _ = ignore(!source_view2#source_buffer#connect#changed ~callback:(fun () -> !source_view2#source_buffer#add_selection_clipboard clipboard))
let _ = ignore(!source_view2#source_buffer#set_language lang)

let get_text_from_selected b () = 
  let get_window_root = 0




  in
  let play () = 
    ignore(Unix.system("timidity "^path^"render.midi"));()
  in
  let buf2 = clipboard#text  in
  let buf = 
  (match buf2 with
    | None -> ""
    | Some x -> x) 
  in
  if (!maths#active) then
    print_endline "MATHS"
  (* check here if it's the list and nat theory or Danny's thing *)
   else
  let buf3 = strip_spaces buf  in
  if (buf3 = "") then () else
  (
    if ((not !music#active) && (not !musicprog#active)) then
    alert "Music theory not specified" 
    else
    let (pre,preroot,post,postroot) =      
    (if b then ("","","","") else
     (if (!music#active) then
       ("","","<c e g>","c")
      else 
	 let lex1 = Lexing.from_string (!source_view1#source_buffer#get_text ()) in
	let lex2 = Lexing.from_string (!source_view2#source_buffer#get_text ()) in
	let m1 = Musicparser.chord1 Musiclexer.token lex1 in
        let m2 = Musicparser.chord1 Musiclexer.token lex2 in
	let mtx1 = (match Chord.findroot m1 with
	  | None -> ("","")
	  | Some x -> Chord.calcnotesfromlist "" "" "" "" x (Chord.calcnoteslist x m1)) in
	let mtx2 = (match Chord.findroot m2 with
	  | None -> ("","")
	  | Some x -> Chord.calcnotesfromlist "" "" "" "" x (Chord.calcnoteslist x m2)) in	
	(fst mtx1,snd mtx1,fst mtx2,snd mtx2)))
    in
    let lex = Lexing.from_string buf3 in
    let music = Musicparser.chord1 Musiclexer.token lex in
    let musictex = 
      (match Chord.findroot music 
       with
	 | None ->
	       (*needs asking here*)
	       (* create window to ask how to get it - from Max's stuff or specify by hand *)
	   let root = get_window_root in
	   Chord.calcnotesfromlist pre preroot post postroot root (Chord.calcnoteslist root music)
	 | Some x ->
           Chord.calcnotesfromlist pre preroot post postroot x (Chord.calcnoteslist x music))
    in
    let pre = r_open_in (path^"render_music_pre.ly") in
    let mid = r_open_in (path^"render_music_mid.ly") in
    let post = r_open_in (path^"render_music_post.ly") in
    let musictexfull = pre^"\n"^(fst musictex)^"\n"^mid^"\n"^(snd musictex)^"\n"^post^"\n" in
    let oc = open_out (path^"render.ly") in
    Printf.fprintf oc "%s" musictexfull;
    close_out oc;
    ignore(Unix.system("cd "^path^";lilypond render.ly"));
    ignore(Unix.system("cpdf -scale-page \"5 5\" "^path^"render.pdf -o render2.pdf"));
    ignore(Unix.system("convert -trim "^path^"render2.pdf "^path^"render.jpg")); 
    let w = GWindow.dialog ~allow_grow:true ~allow_shrink:true ~border_width:10 ~width:600 ~height:440 ~modal:true ~title:"Chord blend result" () in
    let bo = GButton.button ~packing:w#action_area#add  () in
    let _ = xpm_label_box ~file:(path^"play.xpm") ~text:"play" ~packing:bo#add () in
    let _ = GMisc.image ~file:(path^"render.jpg") ~packing: w#vbox#pack () in
    let _ = bo#connect#clicked ~callback:(play) in
    w#show ())

    
let selection_changed (model:#GTree.model) selection = 
  let pr path = 
    let arr = GTree.Path.get_indices path in
    arr.(0)
  in
  pr (List.hd (selection#get_selected_rows))
    
let call_hets s () = 
  print_endline ("hets -g "^s);
  ignore(Unix.system("hets -g "^s));()

let show_new_window t () =
  let trunc s = 
    (String.sub s 0 ((String.length s)-11))^".casl"
  in 
  let win2 = GWindow.window ~allow_grow:true ~allow_shrink:true ~border_width:10 ~modal:true ~width:800 ~height:600 ~title:"CoInvent" () in
  let vbox2 = GPack.vbox ~packing:win2#add () in
  let vp = GPack.paned `VERTICAL ~packing:vbox2#add () in
  let vb = GPack.vbox ~packing:vp#add1 () in
  let frame_sub = GBin.frame ~packing:vb#pack () in
  let bbox_sub = GPack.button_box `HORIZONTAL ~border_width:15 ~layout:`SPREAD
    ~height:50 ~child_height:20 ~child_width:50 ~spacing:20 ~packing:frame_sub#add () in
  let button_rendersub = GButton.button ~packing:bbox_sub#add  () in
  let button_add = GButton.button ~packing:bbox_sub#add  () in
  let button_exit = GButton.button ~packing:bbox_sub#add  () in
  let button_hets = GButton.button ~packing:bbox_sub#add  () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Render" ~packing:button_rendersub#add () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Export" ~packing:button_add#add () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Show in HETS" ~packing:button_hets#add () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Exit" ~packing:button_exit#add () in
  let scrolled_winnew = (GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ~packing:vp#add2
     ())
  in
  let source_viewnew =
  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:scrolled_winnew#add 
    ()) in
let _ = ignore(source_viewnew#source_buffer#set_language lang) in
  let _ = ignore(source_viewnew#source_buffer#connect#changed ~callback:(fun () -> source_viewnew#source_buffer#add_selection_clipboard clipboard)) in
  let s2 = trunc t in
  let buf = r_open_in t in
  source_viewnew#buffer#set_text buf;
  let _ = button_rendersub#connect#clicked ~callback:(get_text_from_selected false) in
  let _ = button_add#connect#clicked ~callback:(nothing) in
  let _ = button_hets#connect#clicked ~callback:(call_hets s2) in
  let _ =button_exit#connect#clicked ~callback:(win2#destroy) in
  win2#show ()

let on_row_activated t c m n = 
  let arr=GTree.Path.get_indices m in
  let t = (snd (List.nth c arr.(0))) in 
  show_new_window t ()


let create_list_blends choices () =
  let cols_lem = new GTree.column_list in
  let str_col_lem = cols_lem#add Gobject.Data.string in
  (* Create a new scrolled window, with scrollbars only if needed *)

  let model = GTree.list_store cols_lem in
  let treeview = GTree.view ~model ~packing:(!scrolled_window#add_with_viewport) () in
  model#clear ();
  List.iter
    (fun x -> let iter = model#append () in
	      model#set ~row:iter ~column:str_col_lem (Printf.sprintf "%s" (fst x)))
    choices;
  (* get mutate results here *)
  let renderer = GTree.cell_renderer_text [] in
  let column = GTree.view_column ~title:"Computed Blends"
    ~renderer:(renderer, ["text", str_col_lem]) () in
  treeview#selection#set_mode `SINGLE;  (* could do multiple here *)
  ignore(treeview#connect#row_activated ~callback:(fun m n -> on_row_activated treeview choices m n));
  ignore(treeview#append_column column);
  !scrolled_window#coerce
    
let add_blends () =
  let preamble = r_open_in (path^"music_casl_pre.casl") in
  let in1 = !source_view1#source_buffer#get_text () in
  let in2 = !source_view2#source_buffer#get_text () in
  let oc = open_out ("/home/ewen/coinvent/Amalgamation/music_blend.casl") in
  Printf.fprintf oc "%s" (preamble^"\n"^in1^"\n"^in2^"\n");
  close_out oc;
  sp1 := Nameparser.chord1 Namelexer.token (Lexing.from_string in1);
  sp2 := Nameparser.chord1 Namelexer.token (Lexing.from_string in2);
  let fn = path^"cached/blendFiles"^(!sp1)^(!sp2)^".txt" in
  if ((Sys.file_exists fn) && !cached#active)  then
    (  let list2 = get_lines2 fn in  
  (* try *)
  (*    vpain_all#remove vpain_all#child2 *)
  (* with *)
  (*   _ -> (); *)
  List.iter !scrolled_window#remove !scrolled_window#children;
  let list = create_list_blends list2 () in
  vpain_all#add2 list  )
  else
  let settings_preamble = r_open_in (path^"settings_pre.py") in
  let settings_input_file = "inputFile = \"/home/ewen/coinvent/Amalgamation/music_blend.casl\"\n" in
  let input_space_names = "inputSpaceNames = [\""^(!sp1)^"\",\""^(!sp2)^"\"]" in
  let oc = open_out ("/home/ewen/coinvent/Amalgamation/settings.py") in
  Printf.fprintf oc "%s" (settings_preamble^"\n"^settings_input_file^"\n"^input_space_names^"\n");
  close_out oc;
  let _ = ignore(Unix.system("cd /home/ewen/coinvent/Amalgamation/;python /home/ewen/coinvent/Amalgamation/run-blending.py")) in
(*%%% need to call amalgams here*)
  let list2 = get_lines "/home/ewen/coinvent/Amalgamation/blendFiles.txt" in  
  (* try *)
  (*    vpain_all#remove vpain_all#child2 *)
  (* with *)
  (*   _ -> (); *)
  List.iter !scrolled_window#remove !scrolled_window#children;
  let list = create_list_blends list2 () in
  vpain_all#add2 list

let file_ok_sel s filew () = 
  s#source_buffer#set_text "";
  let buf = r_open_in filew#filename in
  s#source_buffer#set_text buf;
  filew#destroy()
  

let file_open_m s () = 
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10 () in
  let _ = filew#ok_button#connect#clicked ~callback:(file_ok_sel s filew) in
  let _ = filew#cancel_button#connect#clicked ~callback:(filew#destroy) in
  filew#show ()

let open1_file () =
   file_open_m !source_view1 ()

let open2_file () =
   file_open_m !source_view2 ()

let create_menu_file ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "Openl" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(open1_file));
  let item = GMenu.menu_item ~label: "Openr" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(open2_file));
  let item = GMenu.menu_item ~label: "Exit" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(GMain.Main.quit))

let create_menu_options ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "External Options" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:nothing);
  let genspace = GMenu.menu_item ~label: "Computation" ~packing:file_menu#append () in
  let submenu = GMenu.menu ~packing:genspace#set_submenu () in
    amalgams := (GMenu.radio_menu_item ~label: "Use Amalgams" ~packing:submenu#append ~active:true ());
    hdtp := (GMenu.radio_menu_item ~label: "Use HDTP" ~packing:submenu#append ~group:!amalgams#group ~active:false ())



let create_menu_theory ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  music := (GMenu.radio_menu_item ~label: "Music (cadence)" ~packing:file_menu#append ~active:false ());
  musicprog := (GMenu.radio_menu_item ~label: "Music (progression)" ~packing:file_menu#append ~group:!music#group ~active:false ());
  maths := (GMenu.radio_menu_item ~label: "Maths" ~packing:file_menu#append ~group:!music#group ~active:false ());
  other := (GMenu.radio_menu_item ~label: "Other" ~packing:file_menu#append ~group:!music#group ~active:true ()); 
  let item = GMenu.menu_item ~label: "Cellular Automata" ~packing:file_menu#append () in ignore (item#connect#activate ~callback:run2dca)

let create_menu_actions ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "Compute Blends" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(add_blends));
  let item = GMenu.menu_item ~label: "Render Selection" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(get_text_from_selected true));
  let item = GMenu.menu_item ~label: "Manage Theory Group" ~packing:file_menu#append () in
  cached := (GMenu.check_menu_item ~label:"Use cached results" ~packing:file_menu#append ());
    ignore (item#connect#activate ~callback:nothing)


let _ = button_open1#connect#clicked ~callback:(open1_file)
let _ = button_open2#connect#clicked ~callback:(open2_file)
let _ = button_blend#connect#clicked ~callback:(add_blends)
let _ = button_render#connect#clicked ~callback:(get_text_from_selected true)
let _ = button_quit#connect#clicked ~callback:(GMain.Main.quit)



let _ = 
  vpain_lem_thm#set_position 450;
  vpain_all#set_position 550;  
  ignore (win#connect#destroy (fun _ -> GMain.quit ()));
  create_menu_file ~packing:file_item#set_submenu ();
  create_menu_options ~packing:options_item#set_submenu ();
  create_menu_theory ~packing:theory_item#set_submenu ();
  create_menu_actions ~packing:action_item#set_submenu ();
  win#show ();
  GMain.Main.main ()
