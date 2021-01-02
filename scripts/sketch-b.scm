;; based on this tutorial by Dave Neary:
;; https://www.gimp.org/tutorials/Photo_To_Sketch/

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

(define (script-fu-sketch-b image background-layer threshold colors)
  (gimp-image-undo-group-start image)
  (gimp-item-set-name background-layer "background")

  (let ((highpass-layer (car (gimp-layer-copy background-layer FALSE)))
        (masked-layer '()))
    ;; highpass layer
    (gimp-image-add-layer image highpass-layer 0)
    (gimp-item-set-name highpass-layer "highpass")
    (gimp-image-set-active-layer image highpass-layer)
    (plug-in-edge RUN-NONINTERACTIVE image highpass-layer 1 3 0)
    (gimp-drawable-equalize highpass-layer FALSE)
    (gimp-drawable-desaturate highpass-layer DESATURATE-LUMINANCE)

    ;; masked layer
    (set! masked-layer (car (gimp-layer-copy highpass-layer FALSE)))
    (gimp-image-add-layer image masked-layer 0)
    (gimp-item-set-name masked-layer "masked")
    (gimp-image-set-active-layer image masked-layer)
    (gimp-layer-set-mode masked-layer LAYER-MODE-MULTIPLY)
    (gimp-drawable-invert masked-layer TRUE)

    ;; clean up the highpass layer
    (gimp-image-set-active-layer image highpass-layer)
    (gimp-drawable-curves-spline highpass-layer HISTOGRAM-VALUE 10 (list->vector (list
                                                                                  0.0 0.0
                                                                                  (- (- 1 threshold) 0.1) 0.0
                                                                                  (- 1 threshold) 0.2
                                                                                  (+ (- 1 threshold) 0.1) 0.9
                                                                                  1.0 1.0)))
    (gimp-edit-copy highpass-layer)

    ;; apply mask to the highpass layer
    (gimp-image-set-active-layer image masked-layer)
    (gimp-layer-add-mask masked-layer (car (gimp-layer-create-mask masked-layer ADD-MASK-WHITE)))
    (gimp-edit-paste (car (gimp-layer-get-mask masked-layer)) TRUE)
    (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel image)))
    (gimp-image-remove-layer image highpass-layer)

    (gimp-image-set-active-layer image background-layer)
    (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE colors FALSE TRUE "")
    (gimp-image-convert-rgb image)

    (set! background-layer (car (gimp-image-merge-down image masked-layer EXPAND-AS-NECESSARY))))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))


(script-fu-register
 "script-fu-sketch-b"                     ; func name
 "Sketch B"                               ; menu label
 "Convert an image into a sketch."        ; description
 "Ian Martins"                            ; author
 "2020, Dave Neary, Ian Martins"          ; copyright notice
 "December 7, 2020"                       ; date created
 ""                                       ; image type that the script works on
 SF-IMAGE      "Image"      0             ; the image
 SF-DRAWABLE   "Drawable"   0             ; the layer
 SF-ADJUSTMENT "Line Threshold" '(0.8 0.1 0.9 0.1 0.2 1 0)
 SF-ADJUSTMENT "Colors"         '(20 3 64 1 10 0 0))
(script-fu-menu-register "script-fu-sketch-b" "<Image>/Filters/Artistic")
