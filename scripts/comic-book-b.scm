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

(define (script-fu-comic-book-b image background-layer
                                colors smoothness lightness detail line-weight)
  (gimp-image-undo-group-start image)

  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (scale? (or (< width 1000)
                     (< height 1000))))
    (when scale?
      (gimp-image-scale image (* width 2) (* height 2))
      (plug-in-unsharp-mask RUN-NONINTERACTIVE image background-layer 3 0.5 0))

    (if (> lightness 0)
        (plug-in-softglow RUN-NONINTERACTIVE image background-layer 5 (* lightness 0.2) 0.5))

    (let ((sketch-layer (car (gimp-layer-copy background-layer FALSE))))
      (gimp-image-add-layer image sketch-layer 0)
      (gimp-item-set-name sketch-layer "sketch")
      (gimp-image-set-active-layer image sketch-layer)
      
      (gimp-drawable-equalize sketch-layer FALSE)
      (plug-in-edge RUN-NONINTERACTIVE image sketch-layer 2 3 0)
      (gimp-drawable-desaturate sketch-layer DESATURATE-LUMINANCE)
      
      (let* ((detail-val (- 0.4 detail))
             (line-weight-val (or (and (< (+ detail-val line-weight) 1)
                                       (- 1 line-weight))
                                  (- 1 detail-val))))
        (when (or (> detail-val 0)
                  (< line-weight-val 1))
          (gimp-drawable-levels sketch-layer
                                HISTOGRAM-VALUE
                                detail-val
                                line-weight-val
                                TRUE 1 0 1 TRUE)))
      (gimp-layer-set-mode sketch-layer LAYER-MODE-SUBTRACT)

      (gimp-image-set-active-layer image background-layer)
      (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE colors FALSE TRUE "")
      (let ((count 0))
        (while (< count smoothness)
             (plug-in-sel-gauss RUN-NONINTERACTIVE image background-layer 3 80)
             (set! count (+ count 1))))
      (gimp-image-convert-rgb image)
      (if (> lightness 0)
          (gimp-drawable-levels background-layer
                                HISTOGRAM-VALUE
                                (* lightness 0.1)
                                (- 1 (* lightness 0.4))
                                TRUE 1 (* lightness 0.4) 1 TRUE))
      (gimp-drawable-hue-saturation background-layer HUE-RANGE-ALL 0 0 15 0)

      (set! background-layer (car (gimp-image-merge-down image sketch-layer EXPAND-AS-NECESSARY)))
      )

    (if scale?
        (gimp-image-scale image width height)))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))


(script-fu-register
 "script-fu-comic-book-b"                 ; func name
 "Comic Book B"                           ; menu label
 "Convert an image into a comic."         ; description
 "Ian Martins"                            ; author
 "2020, Ian Martins"                      ; copyright notice
 "December 11, 2020"                      ; date created
 ""                                       ; image type that the script works on
 SF-IMAGE      "Image"      0             ; the image
 SF-DRAWABLE   "Drawable"   0             ; the layer
 SF-ADJUSTMENT "Colors"           '(20 3 64 1 10 0 0)
 SF-ADJUSTMENT "Smoothness"       '(2 0 5 1 1 0 1)
 SF-ADJUSTMENT "Lightness"        '(0.2 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Detail"           '(0.3 0 0.4 0.05 0.1 2 0)
 SF-ADJUSTMENT "Line Weight"      '(0.4 0 0.8 0.1 0.1 2 0))
(script-fu-menu-register "script-fu-comic-book-b" "<Image>/Filters/Artistic")
