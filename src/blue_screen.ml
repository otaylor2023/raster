open Core

let _get_radius_mean ~x ~y image ~radius =
  let x_start = Int.max (x - radius) 0 in
  let y_start = Int.max (y - radius) 0 in
  let x_end = Int.min (x + radius) (Image.width image - 1) in
  let y_end = Int.min (y + radius) (Image.height image - 1) in
  Image.slice image ~x_start ~x_end ~y_start ~y_end |> Image.mean_pixel
;;

let is_blue pixel mean_blue ~multiple =
  let red, green, blue = pixel in
  blue > Int.of_float ((Float.of_int red +. Float.of_int green) *. multiple)
  || blue > mean_blue / 2
;;

let is_regular pixel mean_blue ~multiple =
  let red, green, blue = pixel in
  red + green > Int.of_float (Float.of_int blue *. multiple)
  || blue < Int.of_float (Float.of_int mean_blue *. 0.5)
;;

(* let blue_surroundings ~x ~y image radius = let pixels = get_radius_pixels
   ~x ~y image radius in List.fold pixels ~init:0 ~f:(fun pixel ->

   ) ;; *)

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~(background : Image.t) : _ =
  (* let blue_multiple = 0.9 in *)
  (* let not_blue_multiple = 2. in *)
  let _radius = 5 in
  let max_val = Image.max_val foreground in
  let mean_blue = Pixel.blue (Image.mean_pixel foreground) in
  let () = print_s [%message (Image.mean_pixel foreground : Pixel.t)] in
  Image.mapi foreground ~f:(fun ~x ~y fg_pixel ->
    let bg_pixel = Image.get background ~x ~y in
    let is_very_blue = is_blue fg_pixel mean_blue ~multiple:1.2 in
    let is_regular = is_regular fg_pixel mean_blue ~multiple:0.9 in
    match is_very_blue, is_regular with
    | false, true -> fg_pixel
    | true, false -> bg_pixel
    | _, _ -> max_val, max_val, max_val)
;;

(* let avg_r, avg_g, avg_b = get_radius_average ~x ~y foreground 5 in let
   nearby_blue_weight = avg_b - (avg_r + avg_g) in let combined_blue_weight =
   pixel_blue_weight + nearby_blue_weight in (match combined_blue_weight > 0
   with | true -> bg_pixel | false -> fg_pixel)) *)

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

let%expect_test "blue_screen" =
  (* This test uses existing files on the filesystem. *)
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let tr_foreground =
    Image.load_ppm ~filename:"../images/oz_bluescreen.ppm"
  in
  let tr_background = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let tr_image =
    transform ~foreground:tr_foreground ~background:tr_background
  in
  let diff_list =
    Image.foldi ref_image ~init:[] ~f:(fun ~x ~y diff_list pixel ->
      let tr_pixel = Image.get tr_image ~x ~y in
      match Pixel.equal pixel tr_pixel with
      | false -> tr_pixel :: diff_list
      | true -> diff_list)
  in
  (* print_s [%message (List.length diff_list : int)]; *)
  List.iter diff_list ~f:(fun pixel -> print_s [%message (pixel : Pixel.t)]);
  (* print_s [%message (Pixel.equal reference_pixel transformed_pixel :
     bool)]; *)
  (* [%expect {| ("List.length diff_list" 0) |}] *)
  [%expect {| |}]
;;
