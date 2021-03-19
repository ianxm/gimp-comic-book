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

(define (script-fu-comic-layout width height cols pattern)
  ;; (gimp-image-undo-group-start image)

  (let* ((ret (file-glob pattern 1))
         (num-files (car ret))
         (files (cadr ret))
         (outer-margin 50)
         (inner-margin 15)
         (page-width (+ (* width cols) (*  outer-margin 2) (* inner-margin (- cols 1))))
         (rows (ceiling (/ num-files cols)))
         (page-height (+ (* height rows) (*  outer-margin 2) (* inner-margin (- rows 1))))
         (page (car (gimp-image-new page-width page-height RGB)))
         (background (car (gimp-layer-new page page-width page-height RGB-IMAGE "background" 100 LAYER-MODE-NORMAL)))
         (frame-aspect-ratio (/ width height)))
    (gimp-display-new page)
    (gimp-image-insert-layer page background 0 0)
    (gimp-drawable-fill background FILL-WHITE)

    (let ((count 0))
      (while (not (null? files))
           (let* ((filename (car files))
                  (layer (car (gimp-file-load-layer RUN-NONINTERACTIVE page filename)))
                  (layer-width (car (gimp-drawable-width layer)))
                  (layer-height (car (gimp-drawable-height layer)))
                  (row (floor (/ count cols)))
                  (col (modulo count cols))
                  (image-aspect-ratio (/ (car (gimp-drawable-width layer)) (car (gimp-drawable-height layer))))
                  (xoffset (+ outer-margin (* col (+ inner-margin width))))
                  (yoffset (+ outer-margin (* row (+ inner-margin height)))))
             (gimp-image-insert-layer page layer 0 0)
             (if (> image-aspect-ratio frame-aspect-ratio)
                 ;; clip sides
                 (let* ((new-width (* layer-height frame-aspect-ratio))
                        (diff (- layer-width new-width)))
                   (gimp-layer-resize layer new-width layer-height (- (/ diff 2)) 0))
                 ;; clip top and bottom
                 (let* ((new-height (/ layer-width frame-aspect-ratio))
                        (diff (- layer-height new-height)))
                   (gimp-layer-resize layer layer-width new-height 0 (- (/ diff 3)))))
             (gimp-layer-scale layer width height FALSE)
             (gimp-layer-translate layer (- xoffset (car (gimp-drawable-offsets layer))) (- yoffset (cadr (gimp-drawable-offsets layer))))
             (set! count (+ count 1))
             (set! files (cdr files)))))

    ;; (gimp-image-undo-group-end image)
    (gimp-displays-flush)))


(script-fu-register
 "script-fu-comic-layout"                 ; func name
 "Comic Book Layout"                      ; menu label
 "Lay out images on a comic book page."   ; description
 "Ian Martins"                            ; author
 "2021, Ian Martins"                      ; copyright notice
 "March 18, 2021"                         ; date created
 ""                                       ; image type that the script works on
 SF-VALUE "Frame Width"     "600"
 SF-VALUE "Frame Height"    "450"
 SF-ADJUSTMENT "Columns"    '(2 1 6 1 1 0 0)
 SF-STRING "Path Glob"      "/home/ian/pics/willspics/*_comic.jpg")
 ;; SF-STRING "Path Glob"      "/path/to/images*.jpg")
(script-fu-menu-register "script-fu-comic-layout" "<Image>/Filters")
