type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;

(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  let cBLACK = 1. in
  let cWHITE = 0. in
  List.map  (fun row ->
    (*Sets the greyscale image pixels to be either white or black*)
    List.map (fun fraction_grey ->
      if fraction_grey <= threshold then cWHITE
      else cBLACK) row) img

(* show the image *)
let depict img =
  Graphics.open_graph ""; Graphics.clear_graph ();
  let width, height = List.length (List.hd img), List.length img
      in Graphics.resize_window width height;
  let depict_pix fraction_grey r_index c_index =
      let rgb_grey = int_of_float (255. *. (1. -. fraction_grey))
      in Graphics.set_color (Graphics.rgb rgb_grey rgb_grey rgb_grey);
      plot rgb_grey (height - r_index) in
  (*Runs depict_pix on each pixel, drawing row by row*)
  List.iteri (fun r_index row ->
  List.iteri (fun c_index pix -> depict_pix pix r_index c_index) row) img;
  Unix.sleep 2; Graphics.close_graph () ;;

(* dither max image -- dithered image *)
let dither img =
  List.map
    (fun row ->
     List.map
       (fun v -> if v > Random.float 1.
                 then 1.
                 else 0.) row)
    img

let mona = Monalisa.image ;;

  depict mona ;;

  let mona_threshold = threshold mona 0.75 ;;
    depict mona_threshold ;;

    let mona_dither = dither mona ;;
      depict mona_dither ;;

