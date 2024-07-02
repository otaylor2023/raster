open Core

let distribute_error image ~x ~y error : unit =
  let value_list =
    [ x + 1, y, 7. /. 16.
    ; x - 1, y + 1, 3. /. 16.
    ; x, y + 1, 5. /. 16.
    ; x + 1, y + 1, 1. /. 16.
    ]
  in
  List.iter value_list ~f:(fun (current_x, current_y, multiple) ->
    if current_x > 0
       && current_x < Image.width image - 1
       && current_y > 0
       && current_y < Image.height image - 1
    then (
      let current_value =
        Pixel.red (Image.get image ~x:current_x ~y:current_y)
      in
      let offset = Int.of_float (Float.of_int error *. multiple) in
      let new_value =
        Int.min (Int.max (current_value + offset) 0) (Image.max_val image)
      in
      let new_pixel = new_value, new_value, new_value in
      Image.set image ~x:current_x ~y:current_y new_pixel))
;;

(* let bl_pixel = (x - 1, y + 1) in let below_pixel = (x, y + 1) in let
   br_pixel = (x + 1, y + 1) in *)

(* This should look familiar by now! *)
let transform image =
  let grayscale_image = Grayscale.transform image in
  let gs_copy = Image.copy grayscale_image in
  let max_value = Image.max_val grayscale_image in
  print_s [%message (max_value : int)];
  let _new_image =
    Image.mapi grayscale_image ~f:(fun ~x ~y pixel ->
      let pixel_value = Pixel.red (Image.get gs_copy ~x ~y) in
      let new_value =
        match pixel_value > max_value / 2 with
        | true -> max_value
        | false -> 0
      in
      let error = pixel_value - new_value in
      distribute_error gs_copy ~x ~y error;
      (* print_s [%message (new_value : int )]; *)
      Image.set gs_copy ~x ~y (new_value, new_value, new_value);
      pixel)
  in
  gs_copy
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

let%expect_test "dither" =
  (* This test uses existing files on the filesystem. *)
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  let transformed_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  let _image =
    Image.mapi ref_image ~f:(fun ~x ~y original_pixel ->
      let transformed_pixel = Image.get transformed_image ~x ~y in
      match Pixel.equal original_pixel transformed_pixel with
      | false ->
        print_s
          [%message (original_pixel : Pixel.t) (transformed_pixel : Pixel.t)];
        original_pixel
      | true -> original_pixel)
  in
  ();
  [%expect {| |}]
;;
