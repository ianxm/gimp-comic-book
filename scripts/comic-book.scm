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
                              blur-cycles lightness detail fine-detail shading)
  ;; (gimp-image-undo-group-start image)

  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (min-length 1500)
         (sf 1)
         (selection -1))

    (if (eqv? (car (gimp-selection-is-empty image)) TRUE)
        (set! selection -1)
        (begin 
          (set! selection (car (gimp-selection-save image)))
          (gimp-selection-none image)))

    (when (< (max width height) min-length)
      (set! sf (min 5 (/ min-length (max width height)))))
    (when (<> sf 1)
      (gimp-image-scale image (* width sf) (* height sf))
      (when (> sf 1.2)
        (plug-in-unsharp-mask RUN-NONINTERACTIVE image background-layer 3 0.5 0)))

    (let ((count 0)
          (blur-strength (+ blur-cycles 1)))
      (while (< count blur-cycles)
             (plug-in-gauss RUN-NONINTERACTIVE image background-layer blur-strength blur-strength 0)
             (plug-in-unsharp-mask RUN-NONINTERACTIVE image background-layer (- blur-strength 1) 0.3 0.3)
             (set! count (+ count 1))))

    (when (> lightness 0.0001)
      (gimp-drawable-curves-spline background-layer HISTOGRAM-VALUE 10 (list->vector (list
                                                                                      0.0 0.0
                                                                                      0.05 0.0
                                                                                      0.2 (+ 0.2 (* lightness 0.2))
                                                                                      0.5  (+ 0.5 (* lightness 0.05))
                                                                                      1.0 1.0)))
      (plug-in-softglow RUN-NONINTERACTIVE image background-layer 5 (* lightness 0.2) 0.5))

    (let* ((trace-layer (car (gimp-layer-copy background-layer FALSE)))
           (sketch-layer (car (gimp-layer-copy background-layer FALSE))))
      (when (> fine-detail 0.0001)
        (gimp-image-add-layer image trace-layer 0)
        (gimp-item-set-name trace-layer "trace")
        (gimp-image-set-active-layer image trace-layer)
      
        (when (<> selection -1)
          (let ((mask (car (gimp-layer-create-mask trace-layer ADD-MASK-WHITE)))
                (p-bg (car (gimp-context-get-background)))
                (p-fg (car (gimp-context-get-foreground)))
                (p-metric (car (gimp-context-get-distance-metric)))
                (p-grad (car (gimp-context-get-gradient))))
            (gimp-image-select-item image CHANNEL-OP-ADD selection)
            (gimp-layer-add-mask trace-layer mask)
            (gimp-layer-set-edit-mask trace-layer TRUE)
            (gimp-context-set-background '(0 0 0))
            (gimp-context-set-foreground '(255 255 255))
            (gimp-context-set-distance-metric 0)
            (gimp-context-set-gradient-fg-bg-rgb)
            (gimp-drawable-edit-gradient-fill mask GRADIENT-SHAPEBURST-SPHERICAL 0 FALSE 1 0 TRUE 0 0 1 1)
            (gimp-selection-none image)
            ;; revert settings
            (gimp-layer-set-edit-mask trace-layer FALSE)
            (gimp-context-set-background p-bg)
            (gimp-context-set-foreground p-fg)
            (gimp-context-set-distance-metric p-metric)
            (gimp-context-set-gradient p-grad)))
      
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
        (plug-in-unsharp-mask RUN-NONINTERACTIVE image sketch-layer 2 0.5 0)
      
        (gimp-layer-set-mode sketch-layer LAYER-MODE-MULTIPLY))

      (when (> shading 0.0001)
        (let* ((hatching-layer (car (gimp-layer-new image width height RGB-IMAGE
                                                    "" 100 LAYER-MODE-MULTIPLY)))
               (shading-layer-pre (car (gimp-layer-copy background-layer FALSE)))
               (dark-layer 0)
               ;; (shadows-layer 0)
               (layer-name "light shading")
               (cutoff shading)
               (pct 0.5)
               (length 50))
          (gimp-image-add-layer image shading-layer-pre 0)
          (gimp-image-set-active-layer image shading-layer-pre)
          (gimp-drawable-shadows-highlights shading-layer-pre -40 0)
          (plug-in-gauss RUN-NONINTERACTIVE image shading-layer-pre 3 3 0)
          (set! dark-layer (car (gimp-layer-copy shading-layer-pre FALSE)))
          (gimp-image-add-layer image dark-layer 0)
          (gimp-image-set-active-layer image dark-layer)
          (gimp-drawable-threshold dark-layer HISTOGRAM-VALUE 0.08 cutoff)
          
          ;; (set! shadows-layer (car (gimp-layer-copy shading-layer-pre FALSE)))
          ;; (gimp-image-add-layer image shadows-layer 0)
          ;; (gimp-image-set-active-layer image shadows-layer)
          ;; (gimp-drawable-extract-component shadows-layer 4)
          ;; (gimp-drawable-threshold shadows-layer HISTOGRAM-VALUE 0.50 1)
          ;; (gimp-layer-set-mode shadows-layer LAYER-MODE-MULTIPLY)
          ;; (set! dark-layer (car (gimp-image-merge-down image shadows-layer EXPAND-AS-NECESSARY)))
          ;; (gimp-image-set-active-layer image dark-layer)
          
          (gimp-selection-all image)
          (gimp-edit-copy dark-layer)
          (gimp-selection-none image)
          
          (gimp-image-add-layer image hatching-layer 0)
          (gimp-image-set-active-layer image hatching-layer)
          (gimp-item-set-name hatching-layer layer-name)
          (gimp-drawable-fill hatching-layer FILL-WHITE)
          (plug-in-randomize-hurl RUN-NONINTERACTIVE image hatching-layer pct 1 FALSE 0)
          (plug-in-mblur RUN-NONINTERACTIVE image hatching-layer 0 length 135 0 0)
          (gimp-drawable-desaturate hatching-layer DESATURATE-LUMINANCE)
          (gimp-drawable-levels hatching-layer HISTOGRAM-VALUE 0.99 1 TRUE 1 0 1 TRUE)
          (gimp-drawable-threshold hatching-layer HISTOGRAM-VALUE 0.98 1)
          
          (let ((mask (car (gimp-layer-create-mask hatching-layer ADD-MASK-WHITE)))
                (float 0))
            (gimp-layer-add-mask hatching-layer mask)
            (gimp-layer-set-edit-mask hatching-layer TRUE)
            (set! float (car (gimp-edit-paste mask TRUE)))
            (gimp-floating-sel-anchor float))
          
          (gimp-image-remove-layer image dark-layer)
          (gimp-layer-set-mode hatching-layer LAYER-MODE-MULTIPLY)
      
          (set! hatching-layer (car (gimp-layer-new image width height RGB-IMAGE
                                                    "" 100 LAYER-MODE-MULTIPLY)))
          (set! layer-name "dark shading")
          (set! cutoff (/ cutoff 2))
          (set! pct 1.0)
          (set! length 100)
          (set! dark-layer (car (gimp-layer-copy shading-layer-pre FALSE)))
          (gimp-image-add-layer image dark-layer 0)
          (gimp-image-set-active-layer image dark-layer)
          (gimp-drawable-threshold dark-layer HISTOGRAM-VALUE 0.08 cutoff)
          
          ;; (set! shadows-layer (car (gimp-layer-copy shading-layer-pre FALSE)))
          ;; (gimp-image-add-layer image shadows-layer 0)
          ;; (gimp-image-set-active-layer image shadows-layer)
          ;; (gimp-drawable-extract-component shadows-layer 4)
          ;; (gimp-drawable-threshold shadows-layer HISTOGRAM-VALUE 0.50 1)
          ;; (gimp-layer-set-mode shadows-layer LAYER-MODE-MULTIPLY)
          ;; (set! dark-layer (car (gimp-image-merge-down image shadows-layer EXPAND-AS-NECESSARY)))
          ;; (gimp-image-set-active-layer image dark-layer)
          
          (gimp-selection-all image)
          (gimp-edit-copy dark-layer)
          (gimp-selection-none image)
          
          (gimp-image-add-layer image hatching-layer 0)
          (gimp-image-set-active-layer image hatching-layer)
          (gimp-item-set-name hatching-layer layer-name)
          (gimp-drawable-fill hatching-layer FILL-WHITE)
          (plug-in-randomize-hurl RUN-NONINTERACTIVE image hatching-layer pct 1 FALSE 0)
          (plug-in-mblur RUN-NONINTERACTIVE image hatching-layer 0 length 135 0 0)
          (gimp-drawable-desaturate hatching-layer DESATURATE-LUMINANCE)
          (gimp-drawable-levels hatching-layer HISTOGRAM-VALUE 0.99 1 TRUE 1 0 1 TRUE)
          (gimp-drawable-threshold hatching-layer HISTOGRAM-VALUE 0.98 1)
          
          (let ((mask (car (gimp-layer-create-mask hatching-layer ADD-MASK-WHITE)))
                (float 0))
            (gimp-layer-add-mask hatching-layer mask)
            (gimp-layer-set-edit-mask hatching-layer TRUE)
            (set! float (car (gimp-edit-paste mask TRUE)))
            (gimp-floating-sel-anchor float))
          
          (gimp-image-remove-layer image dark-layer)
          (gimp-layer-set-mode hatching-layer LAYER-MODE-MULTIPLY)
      
          (gimp-image-remove-layer image shading-layer-pre)
          ))

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
      
            ;; index face colors
            (gimp-image-select-item image CHANNEL-OP-ADD selection)
            (gimp-edit-copy background-layer)
            (gimp-selection-all secondary-image)
            (gimp-edit-clear secondary-layer)
            (let ((float (car (gimp-edit-paste secondary-layer FALSE))))
              (gimp-floating-sel-anchor float))
            (gimp-image-convert-indexed secondary-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-face-colors FALSE TRUE "")
            (set! face-colors (script-fu-comic-extract-colormap (gimp-image-get-colormap secondary-image)))
            (gimp-image-convert-rgb secondary-image)
            
            ;; index background colors
            (gimp-selection-invert image)
            (gimp-edit-copy background-layer)
            (gimp-selection-all secondary-image)
            (gimp-edit-clear secondary-layer)
            (let ((float (car (gimp-edit-paste secondary-layer FALSE))))
              (gimp-floating-sel-anchor float))
            (gimp-image-convert-indexed secondary-image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE num-background-colors FALSE TRUE "")
            (set! background-colors (script-fu-comic-extract-colormap (gimp-image-get-colormap secondary-image)))
            (gimp-image-remove-layer secondary-image secondary-layer)
            (gimp-image-delete secondary-image)
      
            ;; prune excess colors
            (let* ((face-prune-range 255)
                   (bw-prune-range 255)
                   (black '(0 0 0))
                   (white '(255 255 255))
                   (c1 face-colors)
                   (c2 '()))
              ;; find prune range
              (while (not (null? c1))
                     (set! c2 (cdr c1))
                     (while (not (null? c2))
                            (set! face-prune-range (min (script-fu-comic-dist (car c1) (car c2)) face-prune-range))
                            (set! c2 (cdr c2)))
                     (set! c1 (cdr c1)))
              (set! face-prune-range (/ face-prune-range 2))
              (set! bw-prune-range (/ face-prune-range 8))
            
              ;; remove black and white from face colors
              (set! face-colors (foldr (lambda (x y)
                                         (if (or (< (script-fu-comic-dist y black) bw-prune-range)
                                                 (< (script-fu-comic-dist y white) bw-prune-range))
                                             x
                                             (cons y x)))
                                       '()
                                       face-colors))
            
              ;; remove black, white and any colors within prune-range of face colors from background colors
              (set! background-colors (foldr (lambda (x y) ; y is current item, x is list
                                               (if (or (< (script-fu-comic-dist y black) bw-prune-range)
                                                       (< (script-fu-comic-dist y white) bw-prune-range)
                                                       (any? (lambda (z) ; z is face point
                                                               (< (script-fu-comic-dist y z) face-prune-range))
                                                             face-colors))
                                                   x
                                                   (cons y x)))
                                             '()
                                             background-colors)))
      
            ;; combine colors in new palette
            (gimp-selection-none image)
            (let ((palette-name (car (gimp-palette-new "comic")))
                  (index 0))
            
              (gimp-palette-add-entry palette-name "m0" '(0 0 0))
              (gimp-palette-add-entry palette-name "m1" '(255 255 255))
            
              (for-each (lambda (x)
                          (gimp-palette-add-entry palette-name (string-append "f" (number->string index)) x)
                          (set! index (+ index 1)))
                        face-colors)
              (set! index 0)
              (for-each (lambda (x)
                          (gimp-palette-add-entry palette-name (string-append "b" (number->string index)) x)
                          (set! index (+ index 1)))
                        background-colors)
              (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-CUSTOM 0 FALSE TRUE palette-name)
              (gimp-palette-delete palette-name))
            ))
      
      (plug-in-median-blur RUN-NONINTERACTIVE image background-layer
                           (+ 1 smoothness (floor (/ (max width height) 800)))
                           50)
      
      (gimp-image-set-active-layer image sketch-layer)
      (plug-in-median-blur RUN-NONINTERACTIVE image sketch-layer 1 50)
      
      (gimp-image-set-active-layer image background-layer)
      (gimp-image-convert-rgb image)
      (when (> lightness 0.0001)
        (gimp-drawable-hue-saturation background-layer HUE-RANGE-ALL 0 0 (+ (* lightness 20) 12) 0))

      (gimp-drawable-levels trace-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)
      (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)

      (when (<> sf 1)
        (gimp-image-scale image width height))

      (set! background-layer (car (gimp-image-flatten image))))

    (when (<> selection -1)
      (gimp-image-select-item image CHANNEL-OP-ADD selection)
      (gimp-image-remove-channel image selection)))

  ;; (gimp-image-undo-group-end image)
  (gimp-displays-flush))

(define (script-fu-comic-dist a b)
  "Compute distance between three dimensional points A and B"
  (sqrt (+  (expt (- (nth 0 b) (nth 0 a)) 2)
            (expt (- (nth 1 b) (nth 1 a)) 2)
            (expt (- (nth 2 b) (nth 2 a)) 2))))

(define (script-fu-comic-extract-colormap colormap)
  "Convert a COLORMAP into a list of colors"
  (let ((index 0)
        (colors '()))
    (while (< index (/ (car colormap) 3))
           (set! colors (cons
                         (list (aref (cadr colormap) (+ 0 (* index 3)))
                               (aref (cadr colormap) (+ 1 (* index 3)))
                               (aref (cadr colormap) (+ 2 (* index 3))))
                         colors))
           (set! index (+ index 1)))
    colors))

(define (any? pred lst)
"True if PRED is true for any item in the LST"
  (let ((item lst)
        (ret #f))
    (while (not (null? item))
           (if (apply pred (list (car item)))
               (begin (set! item nil)
                      (set! ret #t))
               (set! item (cdr item))))
    ret))

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
 SF-ADJUSTMENT "Blur Cycles"          '(1 0 6 1 1 0 1)
 SF-ADJUSTMENT "Lightness"            '(0.1 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Detail"               '(0.5 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Fine Detail"          '(0.5 0 1 0.1 0.2 2 0)
 SF-ADJUSTMENT "Shading"              '(0.3 0 1 0.1 0.2 2 0))
(script-fu-menu-register "script-fu-comic-book" "<Image>/Filters/Artistic")
