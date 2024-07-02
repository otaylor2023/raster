open! Core

let is_inbounds ~x ~y image =
  x > 0 && x < Image.width image - 1 && y > 0 && y < Image.height image - 1
;;

let calculate_g (g_arr : int list) ~x ~y image =
  let coordinates =
    [ x - 1, y - 1
    ; x, y - 1
    ; x + 1, y - 1
    ; x - 1, y
    ; x, y
    ; x + 1, y
    ; x - 1, y + 1
    ; x, y + 1
    ; x + 1, y + 1
    ]
  in
  List.fold2_exn
    g_arr
    coordinates
    ~init:0
    ~f:(fun g_val kernel_elt (image_x, image_y) ->
      (* let (image_x, image_y) = List.nth_exn (List.nth_exn y) x in *)
      match is_inbounds ~x:image_x ~y:image_y image with
      | false -> g_val
      | true ->
        let pixel_value =
          Pixel.red (Image.get ~x:image_x ~y:image_y image)
        in
        let product = pixel_value * kernel_elt in
        product + g_val)
;;

let transform image (threshold : float) =
  let gs_image = Grayscale.transform image |> Blur.transform ~radius:2 in
  let gx = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
  let gy = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
  let max_val = Image.max_val gs_image in
  let real_threshold = Float.of_int max_val *. threshold in
  let white_pixel = max_val, max_val, max_val in
  let black_pixel = 0, 0, 0 in
  Image.mapi gs_image ~f:(fun ~x ~y _pixel ->
    let gx_val = calculate_g gx ~x ~y gs_image in
    let gy_val = calculate_g gy ~x ~y gs_image in
    let g_val = sqrt (Float.of_int (Int.pow gx_val 2 + Int.pow gy_val 2)) in
    match Float.(g_val > real_threshold) with
    | true -> white_pixel
    | false -> black_pixel)
;;

let command =
  Command.basic
    ~summary:"Detect edges in an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"the threshold for edges"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edges.ppm")]
;;
