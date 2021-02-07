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

(define (script-fu-comic-book-a image background-layer colors lightness)
  (gimp-image-undo-group-start image)

  (if (> lightness 0)
    (plug-in-softglow RUN-NONINTERACTIVE image background-layer 10 (* lightness 0.3) 0.9))
  (plug-in-sel-gauss RUN-NONINTERACTIVE image background-layer 4 0.2)

  (let ((sketch-layer-base (car (gimp-layer-copy background-layer FALSE)))
        (sketch-layer-mask (car (gimp-layer-copy background-layer FALSE))))

    ;; add sketch layers
    (gimp-image-add-layer image sketch-layer-base 0)
    (gimp-item-set-name sketch-layer-base "sketch-base")
    (gimp-image-add-layer image sketch-layer-mask 0)
    (gimp-item-set-name sketch-layer-mask "sketch-mask")

    (gimp-image-set-active-layer image sketch-layer-mask)
    (plug-in-edge RUN-NONINTERACTIVE image sketch-layer-mask 2 3 0)
    (plug-in-edge RUN-NONINTERACTIVE image sketch-layer-mask 1 3 0)
    (gimp-layer-set-mode sketch-layer-mask LAYER-MODE-DIVIDE)
    (set! sketch-layer-base (car (gimp-image-merge-down image sketch-layer-mask EXPAND-AS-NECESSARY)))

    ;; masked layer
    (set! sketch-layer-mask (car (gimp-layer-copy sketch-layer-base FALSE)))
    (gimp-image-add-layer image sketch-layer-mask 0)
    (gimp-item-set-name sketch-layer-mask "mask")
    (gimp-image-set-active-layer image sketch-layer-mask)
    (gimp-drawable-invert sketch-layer-mask TRUE)
    (gimp-drawable-threshold sketch-layer-mask HISTOGRAM-VALUE 0.9 1)
    (gimp-edit-copy sketch-layer-mask)

    ;; apply mask to sketch
    (gimp-image-set-active-layer image sketch-layer-base)
    (gimp-layer-add-mask sketch-layer-base (car (gimp-layer-create-mask sketch-layer-base ADD-MASK-WHITE)))
    (gimp-edit-paste (car (gimp-layer-get-mask sketch-layer-base)) TRUE)
    (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel image)))
    (gimp-image-remove-layer image sketch-layer-mask)
    (gimp-layer-set-mode sketch-layer-base LAYER-MODE-LINEAR-BURN)

    (gimp-image-set-active-layer image background-layer)
    (if (> lightness 0)
        (gimp-drawable-levels background-layer HISTOGRAM-VALUE
                              0 (- 1 (* lightness 0.5)) TRUE 1
                              (* lightness 0.2) 1 FALSE))
    (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE colors FALSE TRUE "")
    (plug-in-sel-gauss RUN-NONINTERACTIVE image background-layer 3 80)
    (gimp-image-convert-rgb image)

    ;; merge
    (set! background-layer (car (gimp-image-merge-down image sketch-layer-base EXPAND-AS-NECESSARY))))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))


(script-fu-register
 "script-fu-comic-book-a"                 ; func name
 "Comic Book A"                           ; menu label
 "Convert an image into a comic."         ; description
 "Ian Martins"                            ; author
 "2020, Ian Martins"                      ; copyright notice
 "December 7, 2020"                       ; date created
 "RGB* GRAY*"                             ; image type that the script works on
 SF-IMAGE      "Image"      0             ; the image
 SF-DRAWABLE   "Drawable"   0             ; the layer
 SF-ADJUSTMENT "Colors"           '(20 3 64 1 10 0 0)
 SF-ADJUSTMENT "Lightness"        '(0.2 0 1 0.1 0.2 2 0))
(script-fu-menu-register "script-fu-comic-book-a" "<Image>/Filters/Artistic")
