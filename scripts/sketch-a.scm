;; based on this tutorial by Leah Lefler:
;; https://feltmagnet.com/photography/Turning-Photos-Into-Cartoons-A-GIMP-Tutorial

; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (script-fu-sketch-a image layer darken)
  (gimp-image-undo-group-start image)

  (let ((top-layer (car (gimp-layer-copy layer FALSE))))
    (gimp-image-add-layer image top-layer 0)

    (let ((count 0))
      (while (< count darken)
             (plug-in-edge RUN-NONINTERACTIVE image top-layer 1 3 0)
             (set! count (+ count 1))))

    (gimp-layer-set-mode top-layer LAYER-MODE-DIVIDE)
    (set! layer (car (gimp-image-merge-down image top-layer EXPAND-AS-NECESSARY))))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))


(script-fu-register
 "script-fu-sketch-a"                     ; func name
 "Sketch A"                               ; menu label
 "Convert an image into a sketch."        ; description
 "Ian Martins"                            ; author
 "2020, Leah Lefler, Ian Martins"         ; copyright notice
 "December 7, 2020"                       ; date created
 ""                                       ; image type that the script works on
 SF-IMAGE      "Image"      0             ; the image
 SF-DRAWABLE   "Drawable"   0             ; the layer
 SF-ADJUSTMENT "Darken Steps"       '(2 1 4 1 1 0 1))
(script-fu-menu-register "script-fu-sketch-a" "<Image>/Filters/Artistic")
