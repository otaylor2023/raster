open! Core
module Grayscale = Grayscale

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "edges", Edge_detection.command
    ; "mosaic", Mosaic.command
    ]
;;
