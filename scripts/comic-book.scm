;; Comic book filter for GIMP.  Find documentation at https://github.com/ianxm/gimp-comic-book

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

(define (script-fu-comic-book image background-layer
                              colors smoothness lightness
                              detail fine-detail allow-resize?)
  (gimp-image-undo-group-start image)

  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (min-length 1500)
         (max-length 4000))

    (when (= allow-resize? TRUE)
      (cond
       ((< height min-length) (gimp-image-scale image (/ (* width min-length) height) min-length))
       ((< width min-length) (gimp-image-scale image min-length (/ (* height min-length) width)))
       ((> height max-length) (gimp-image-scale image (/ (* width max-length) height) max-length))
       ((> width max-length) (gimp-image-scale image max-length (/ (* height max-length) width))))
      (if (and allow-resize?
               (< (max (* width 2) (* height 2)) max-length))
          (plug-in-unsharp-mask RUN-NONINTERACTIVE image background-layer 3 0.5 0)))

    (if (> lightness 0)
        (plug-in-softglow RUN-NONINTERACTIVE image background-layer 5 (* lightness 0.2) 0.5))

    (let ((sketch-layer (car (gimp-layer-copy background-layer FALSE)))
          (trace-layer (car (gimp-layer-copy background-layer FALSE))))
      (when (> fine-detail 0)
        (gimp-image-add-layer image trace-layer 0)
        (gimp-item-set-name trace-layer "trace")
        (gimp-image-set-active-layer image trace-layer)
      
        (plug-in-edge RUN-NONINTERACTIVE image trace-layer 1 2 0)
        (gimp-drawable-desaturate trace-layer DESATURATE-LUMINANCE)
      
        (let* ((detail-inv (- 1 fine-detail))
               (detail-low (* detail-inv 0.6))   ; range from 0.6 (lowest) to 0 (highest)
               (detail-high (+ detail-low 0.3))) ; range from 0.9 (lowest) to 0.5 (highest)
               (gimp-drawable-levels trace-layer
                                     HISTOGRAM-VALUE
                                     detail-low
                                     detail-high
                                     TRUE 1 0 1 TRUE))
        (gimp-drawable-invert trace-layer TRUE)
        (gimp-layer-set-mode trace-layer LAYER-MODE-MULTIPLY))
      

      (when (> detail 0)
        (gimp-image-add-layer image sketch-layer 0)
        (gimp-item-set-name sketch-layer "sketch")
        (gimp-image-set-active-layer image sketch-layer)
        (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0 1 TRUE 1 0.3 1 TRUE)
        (let* ((detail-inv (- 1 detail))
               (detail-val (+ (* detail-inv 0.4) 0.6))) ; range from 1 (lowest) to 0.6 (highest)
          (plug-in-photocopy RUN-NONINTERACTIVE image sketch-layer 20.0 1.0 1.0 detail-val))
        (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.7 1 TRUE 1 0 1 TRUE)
        (gimp-layer-set-mode sketch-layer LAYER-MODE-MULTIPLY))

      (gimp-image-set-active-layer image background-layer)
      (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE colors FALSE TRUE "")
      
      (let ((count 0))
        (while (< count smoothness)
               (plug-in-sel-gauss RUN-NONINTERACTIVE image background-layer (+ 2 smoothness) 80)
               (set! count (+ count 1))))
      
      (gimp-image-set-active-layer image sketch-layer)
      ;; (plug-in-gauss RUN-NONINTERACTIVE image sketch-layer 2 2 0) ; doesn't work
      (plug-in-sel-gauss RUN-NONINTERACTIVE image sketch-layer 2 255)
      
      (gimp-image-set-active-layer image background-layer)
      (gimp-image-convert-rgb image)
      (when (> lightness 0)
          (gimp-drawable-hue-saturation background-layer HUE-RANGE-ALL 0 0 (+ (* lightness 20) 32) 0)
          (gimp-drawable-levels background-layer
                                HISTOGRAM-VALUE
                                (* lightness 0.1)
                                (- 1 (* lightness 0.2))
                                TRUE 1 (* lightness 0.5) 1 TRUE))

      (gimp-drawable-levels trace-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)
      (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)

      (set! background-layer (car (gimp-image-flatten image))))

    (if (and (= allow-resize? TRUE)
             (< (max width height) min-length))
        (gimp-image-scale image width height)))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))


(script-fu-register
 "script-fu-comic-book"                   ; func name
 "Comic Book"                             ; menu label
 "Convert an image into a comic."         ; description
 "Ian Martins"                            ; author
 "2020, Ian Martins"                      ; copyright notice
 "December 11, 2020"                      ; date created
 ""                                       ; image type that the script works on
 SF-IMAGE      "Image"      0             ; the image
 SF-DRAWABLE   "Drawable"   0             ; the layer
 SF-ADJUSTMENT "Colors"           '(20 3 64 1 10 0 0)
 SF-ADJUSTMENT "Smoothness"       '(2 0 5 1 1 0 1)
 SF-ADJUSTMENT "Lightness"        '(0.3 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Detail"           '(0.5 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Fine Detail"      '(0.5 0 1 0.1 0.2 2 0)
 SF-TOGGLE     "Allow Resize"     TRUE)
(script-fu-menu-register "script-fu-comic-book" "<Image>/Filters/Artistic")