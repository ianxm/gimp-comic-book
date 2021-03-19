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
                              num-face-colors num-background-colors smoothness
                              lightness detail fine-detail allow-resize?)
  (gimp-image-undo-group-start image)

  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (min-length 1200)
         (max-length 4000)
         (sf 1)
         (selection -1))


    (when (= allow-resize? TRUE)
      (cond
       ((<= height min-length)
        (set! sf (/ width height))
        (gimp-image-scale image (* min-length sf) min-length))
       ((>= height max-length)
        (set! sf (/ width height))
        (gimp-image-scale image (* max-length sf) max-length))
       ((<= width min-length)
        (set! sf (/ height width))
        (gimp-image-scale image min-length (* min-length sf)))
       ((>= width max-length)
        (set! sf (/ height width))
        (gimp-image-scale image max-length (* max-length sf))))
      (when (> sf 1.2)
        (plug-in-unsharp-mask RUN-NONINTERACTIVE image background-layer 3 0.5 0)))

    (if (eqv? (car (gimp-selection-is-empty image)) TRUE)
        (set! selection -1)
        (begin 
          (set! selection (car (gimp-selection-save image)))
          (gimp-selection-none image)))

    (when (> lightness 0.0001)
      (gimp-drawable-curves-spline background-layer HISTOGRAM-VALUE 10 (list->vector (list
                                                                                      0.0 0.0
                                                                                      0.05 0.0
                                                                                      0.2 (+ 0.2 (* lightness 0.2))
                                                                                      0.5  (+ 0.5 (* lightness 0.05))
                                                                                      1.0 1.0)))
      (plug-in-softglow RUN-NONINTERACTIVE image background-layer 5 (* lightness 0.2) 0.5))

    (let* ((sketch-layer (car (gimp-layer-copy background-layer FALSE)))
           (trace-layer (car (gimp-layer-copy background-layer FALSE))))
      (when (> fine-detail 0.0001)
        (gimp-image-add-layer image trace-layer 0)
        (gimp-item-set-name trace-layer "trace")
        (gimp-image-set-active-layer image trace-layer)
      
        (gimp-drawable-curves-spline trace-layer HISTOGRAM-VALUE 6 (list->vector (list
                                                                                  0.0 0.0
                                                                                  0.5 0.875
                                                                                  1.0 1.0)))
        (gimp-drawable-desaturate trace-layer DESATURATE-LUMINANCE)
        (plug-in-edge RUN-NONINTERACTIVE image trace-layer 1 2 0)
      
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
      

      (when (> detail 0.0001)
        (gimp-image-add-layer image sketch-layer 0)
        (gimp-item-set-name sketch-layer "sketch")
        (gimp-image-set-active-layer image sketch-layer)
        (gimp-drawable-curves-spline sketch-layer HISTOGRAM-VALUE 10 (list->vector (list
                                                                                    0.0  0.25
                                                                                    0.25 0.375
                                                                                    0.5  0.625
                                                                                    0.75 0.875
                                                                                    1.0  1.0)))
        (let* ((detail-inv (- 1 detail))
               (detail-val (+ (* detail-inv 0.4) 0.6))) ; range from 1 (lowest) to 0.6 (highest)
          (plug-in-photocopy RUN-NONINTERACTIVE image sketch-layer 12.0 1.0 0.0 detail-val))
        (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.7 1 TRUE 1 0 1 TRUE)
      
        (let ((count 0))
          (while (< count 2)
                 (plug-in-unsharp-mask RUN-NONINTERACTIVE image sketch-layer 2 0.5 0)
                 (set! count (+ count 1))))
      
        (gimp-layer-set-mode sketch-layer LAYER-MODE-MULTIPLY))

      (gimp-image-set-active-layer image background-layer)
      (if (= selection -1)
          ;; no selection, just convert
          (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-background-colors FALSE TRUE "")
      
          ;; give selected pixels preferential treatment
          (let* ((width (car (gimp-image-width image)))
                 (height (car (gimp-image-height image)))
                 (face-colors '())
                 (background-colors '())
                 (secondary-image 0)
                 (secondary-layer 0))
      
            (set! secondary-image (car (gimp-image-new width height RGB)))
            (set! secondary-layer (car (gimp-layer-new secondary-image width height RGB-IMAGE "secondary" 100 LAYER-MODE-NORMAL)))
            (gimp-layer-add-alpha secondary-layer)
            (gimp-image-insert-layer secondary-image secondary-layer 0 0)
            ;; (gimp-display-new secondary-image)
      
            (gimp-image-select-item image CHANNEL-OP-ADD selection)
            (gimp-edit-copy background-layer)
            (gimp-selection-all secondary-image)
            (gimp-edit-clear secondary-layer)
            (let ((float (car (gimp-edit-paste secondary-layer TRUE))))
              (gimp-floating-sel-anchor float))
            (gimp-image-convert-indexed secondary-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-face-colors FALSE TRUE "")
            (set! face-colors (gimp-image-get-colormap secondary-image))
            (gimp-image-convert-rgb secondary-image)
      
            (gimp-selection-invert image)
            (gimp-edit-copy background-layer)
            (gimp-selection-all secondary-image)
            (gimp-edit-clear secondary-layer)
            (let ((float (car (gimp-edit-paste secondary-layer TRUE))))
              (gimp-floating-sel-anchor float))
            (gimp-image-convert-indexed secondary-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-background-colors FALSE TRUE "")
            (set! background-colors (gimp-image-get-colormap secondary-image))
            (gimp-image-delete secondary-image)
      
            (gimp-selection-none image)
            (let ((palette-name (car (gimp-palette-new "indexed")))
                  (index 0))
              (gimp-palette-add-entry palette-name (string-append "f" (number->string index)) '(0 0 0))
              (gimp-palette-add-entry palette-name (string-append "f" (number->string index)) '(255 255 255))
              (while (< index num-face-colors)
                     (gimp-palette-add-entry palette-name
                                             (string-append "f" (number->string index))
                                             (list (aref (cadr face-colors) (+ 0 (* index 3)))
                                                   (aref (cadr face-colors) (+ 1 (* index 3)))
                                                   (aref (cadr face-colors) (+ 2 (* index 3)))))
                     (set! index (+ index 1)))
              (set! index 0)
              (while (< index num-background-colors)
                     (gimp-palette-add-entry palette-name
                                             (string-append "b" (number->string index))
                                             (list (aref (cadr background-colors) (+ 0 (* index 3)))
                                                   (aref (cadr background-colors) (+ 1 (* index 3)))
                                                   (aref (cadr background-colors) (+ 2 (* index 3)))))
                     (set! index (+ index 1)))
              (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-CUSTOM 0 FALSE TRUE palette-name))
            )
          )
      
      (let ((count 0))
        (while (< count smoothness)
               (plug-in-median-blur RUN-NONINTERACTIVE image background-layer
                                    (+ 1 smoothness (floor (/ (max width height) 1500)))
                                    50)
               (set! count (+ count 1))))
      
      (gimp-image-set-active-layer image sketch-layer)
      (plug-in-median-blur RUN-NONINTERACTIVE image sketch-layer 1 50)
      
      (gimp-image-set-active-layer image background-layer)
      (gimp-image-convert-rgb image)
      (when (> lightness 0.0001)
          (gimp-drawable-hue-saturation background-layer HUE-RANGE-ALL 0 0 (+ (* lightness 20) 12) 0))

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
 "RGB* GRAY*"                             ; image type that the script works on
 SF-IMAGE      "Image"      0             ; the image
 SF-DRAWABLE   "Drawable"   0             ; the layer
 SF-ADJUSTMENT "Face Colors"          '(5 2 12 1 10 0 0)
 SF-ADJUSTMENT "Background Colors"    '(24 3 64 1 10 0 0)
 SF-ADJUSTMENT "Smoothness"           '(2 0 5 1 1 0 1)
 SF-ADJUSTMENT "Lightness"            '(0.1 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Detail"               '(0.5 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Fine Detail"          '(0.5 0 1 0.1 0.2 2 0)
 SF-TOGGLE     "Allow Resize"         TRUE)
(script-fu-menu-register "script-fu-comic-book" "<Image>/Filters/Artistic")
