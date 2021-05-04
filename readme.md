
# Table of Contents

1.  [Comic Book Filter](#orgfa93dd1)
    1.  [Overview](#org3b819ca)
    2.  [Example](#org4081c6d)
    3.  [Filter](#orgf50008b)
        1.  [General Idea](#org673a93e)
        2.  [Script](#orgde63deb)
    4.  [Previous Attemps](#org1800fed)
        1.  [Sketch A](#org23d3bcc)
        2.  [Sketch B](#org0cf0abc)
        3.  [Comic Book A](#org4aef268)
        4.  [Comic Book B](#org1e7dbb1)
    5.  [References](#org8a190be)
2.  [Literate Programming](#org5e70abf)



<a id="orgfa93dd1"></a>

# Comic Book Filter


<a id="org3b819ca"></a>

## Overview

This is a GIMP filter to convert an image so that it looks like a
frame from a comic book.  This is similar to the many cartoon filters
out there.

WARNING: This filter won't work on a standard build of GIMP.  This
filter uses the `median blur` filter, which is not in the PDB, so it
is not available to scripts in the current GIMP build.  I submitted a
[patch](https://gitlab.gnome.org/GNOME/gimp/-/merge_requests/405) to the GIMP maintainers to include it.  To run this yourself
you'll need to wait for that patch to be accepted or patch and build
GIMP yourself which, unfortunately, is harder than it sounds.


<a id="org4081c6d"></a>

## Example

This is an example photo run through the comic book filter.

Here is the original:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_orig_2.jpg)

Here is the final result:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_comic_3.jpg)

These are the GIMP filter settings I used to convert this image:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_dialog_3.jpg)

In case you're curious, here are the overlay and background layers
that make up the final result:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_overlays_3.jpg)
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_background_2.jpg)


<a id="orgf50008b"></a>

## Filter


<a id="org673a93e"></a>

### General Idea

There are two main parts to the comic book image: lines that
outline shapes and emphasize edges, and colors that fill in the
shapes.

We run two filters to find the lines to overlay over the background
colors.  For the background layer we flatten the colors, then smooth
the image and brighten the colors.

For best results, use the oval select tool to select faces before
running the filter.  This will ensure optimal colors are chosen for
skin tones.

The final script is [here](scripts/comic-book.scm).


<a id="orgde63deb"></a>

### Script

The following sections go through the comic book filter script in
detail.  The script is examined in sections here, but it is combined
into a single script for GIMP.

1.  license

    This is the GPLv3 software license.  We add it near the top of each script.
    
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

2.  register

    This registers the script with GIMP and configures the dialog with the
    parameters that will be passed to the filter.  Everything is
    boilerblate except seven of the parameters, which I'll go over now.
    
    -   **Face Colors:** The number of colors to choose for people's faces
        (only used if there is a selection)
    -   **Background Colors:** The number of colors to choose for the rest of
        the image
    -   **Smoothness:** a higher value results in smoother curves where
        indexed colors meet, which is what you'd expect if the colors were
        drawn by hand
    -   **Blur Cycles:** use a higher value to reduce noise, but higher values
        result in blurred features
    -   **Lightness:** a higher value results in lighter colors
    -   **Detail:** a higher value results in more lines
    -   **Fine Detail:** a higher value results in more thin lines
    -   **Shading:** a higher value results in more diagonal shading lines
    
    The meaning of the values we set is explained in [section 3.4.8 of the
    GIMP doc](https://docs.gimp.org/2.8/en/gimp-using-script-fu-tutorial-first-script.html).
    
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

3.  convert

    This is the top-level function for the Comic Book filter.
    
    We wrap all operations on the image in an undo group so that all of
    the steps show up in GIMP as a single action.  This means that one
    "undo" brings you back to the image before this filter was run.
    
    If there is a selection, we save it to a channel and dismiss it.  It
    is used later when we index the colors.
    
    Edge detection seems to work best on images at least 1500 px wide, so
    we enlarge the image if needed.  If we enlarge an image significantly,
    we also sharpen it because enlarging an image can cause it to blur.
    If we enlarge it at the beginning we shrink it back to its original
    size at the end.
    
    Then we go through `Blur Cycles` steps of blurring and sharpening the
    image.  The overall effect is to reduce noise, which is especially
    problematic in low light photos, but higher values quickly lead to
    blurry results so use this sparingly.
    
    The next thing we do to the image is to lighten it.  We apply `curves`
    and then a `softglow` filter.  We skip both of these if the
    `lightness` parameter was set to zero.  The `curves` operation
    lightens the lighter parts of the image while leaving the darks alone.
    This increases the contrast which helps in edge detection, making the
    lines we will overlay on the image more pronounced.  The `softglow`
    helps to reduce lines in the lighter parts of a photo (often this
    reduces line-noise on faces).  The `lightness` parameter's range is
    from `0` to `1` but we scale it to the range `0` to `0.2` for
    `softglow` to keep it to a reasonable level of glowyness.
    
    Next we create two layers which overlay the background layer with
    lines, then work on the background layer, then come back to darken the
    overlay layers.  These steps are covered in greater detail in the
    sections below.  Once we're done with the overlay and background
    layers we merge them together.
    
    Finally we flush the GIMP display to update the image in the buffer.
    
        (define (script-fu-comic-book image background-layer
                                      num-face-colors num-background-colors smoothness
                                      blur-cycles lightness detail fine-detail shading)
          ;; (gimp-image-undo-group-start image)
        
          (let* ((orig-width (car (gimp-image-width image)))
                 (orig-height (car (gimp-image-height image)))
                 (width orig-width)
                 (height orig-height)
                 (min-length 1500)
                 (sf 1)
                 (selection -1))
        
            (if (eqv? (car (gimp-selection-is-empty image)) TRUE)
                (set! selection -1)
                (begin 
                  (set! selection (car (gimp-selection-save image)))
                  (gimp-selection-none image)))
        
            (when (< (max orig-width orig-height) min-length)
              (set! sf (min 5 (/ min-length (max orig-width orig-height)))))
            (when (<> sf 1)
              (set! width (* width sf))
              (set! height (* height sf))
              (gimp-image-scale image width height)
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
              <<sketch-layer>>
        
              <<trace-layer>>
        
              <<shading-layer>>
        
              <<background-layer>>
        
              <<darken-overlays>>
        
              (when (<> sf 1)
                (gimp-image-scale image orig-width orig-height))
        
              ;; (gimp-item-set-visible background-layer FALSE)
              (set! background-layer (car (gimp-image-flatten image))))
        
            (when (<> selection -1)
              (gimp-image-select-item image CHANNEL-OP-ADD selection)
              (gimp-image-remove-channel image selection)))
        
          ;; (gimp-image-undo-group-end image)
          (gimp-displays-flush))
    
    Here we create a layer that outlines shapes, which we will call the
    sketch layer.  First we create and add the new layer on top of the
    background layer.
    
    Next we use the `photocopy` filter to convert the layer into lines
    where the image is darkest.  This method was based on the
    [cartoon-quick](https://www.gimphelp.org/effects_cartoon_quick.html) filter.  We use the `Detail` parameter to determine how
    sensitive photocopy should be.  This does a good job of marking edges,
    but also results in noise in large dark areas.  To reduce that effect
    we lighten the image with a `curves` operation before the `photocopy`
    call and darken it back after using the `levels` and `sharpen`
    operations.  We also run a `median-blur` on the layer while the image
    is indexed to clear up some of the noise.  If `Detail` is turned down
    to zero we skip this step entirely.
    
    The `photocopy` filter produces an inverted greyscale image so there's
    no need to desaturate or invert the sketch layer.  We just set its
    mode to `MULTIPLY` and are done here.
    
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
    
    Here we create a "trace layer" that traces over lines.  It adds thin
    lines wherever there are edges in the image.  The trace layer usually
    picks up some details that the sketch layer misses.
    
    We duplicate the background and add the new layer to the top.  We
    lighten the new layer with `curves` to wash out any glare or shiny
    spots so they aren't picked up by the edge detection.  We also add a
    layer mask to cut a hole in the layer where there are faces to prevent
    the trace layer from outlining teeth or filling in eyes, both of which
    it has a tendency to do and both are a bad look.  We use a gradient to
    blend the layer out so there aren't sharp edges.
    
    The main work is done by the Sobel Edge Detection filter, which we run
    on the new layer.  We desaturate to convert to greyscale since we
    don't want color info.  Then we adjust levels in the trace layer to
    stengthen the most significant lines and dim the noise.  We use the
    `Fine Detail` parameter to control this adjustment.  If `Fine Detail`
    is turned down to zero, we skip this step entirely.
    
    Finally we invert the trace layer and set its mode to `MULTIPLY` so
    that the lines show up overlayed on the background.
    
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
    
    Now lets add some shading to give it more depth and action.  I copied
    the technique for generating dashed lines from the [Inkpen filter](https://www.gimphelp.org/artist_inkpen.html).  The
    idea is to find the darkest parts of the image and add diagonal dashed
    lines which look like hatching to the image.
    
    We do this twice at different levels of darkness.  We overlay the
    strokes to produce two levels of shading in the comic image.
    
    This looks really good in many cases, but looks bad if the shading
    covers someone's hair, since anyone would shade in the direction of
    the hair instead of just going diagonally.  I've not found a way to
    prevent this, though.
    
        (when (> shading 0.0001)
          (let* ((hatching-layer (car (gimp-layer-new image width height RGB-IMAGE
                                                      "" 100 LAYER-MODE-MULTIPLY)))
                 (shading-layer-pre (car (gimp-layer-copy background-layer FALSE)))
                 (dark-layer 0)
                 (layer-name "light shading")
                 (cutoff shading)
                 (angle 135)
                 (stroke-spacing 0.5)
                 (length 50))
            (gimp-image-add-layer image shading-layer-pre 0)
            (gimp-image-set-active-layer image shading-layer-pre)
            (gimp-drawable-shadows-highlights shading-layer-pre -40 0)
            (plug-in-gauss RUN-NONINTERACTIVE image shading-layer-pre 3 3 0)
            <<shading-step>>
        
            (set! hatching-layer (car (gimp-layer-new image width height RGB-IMAGE
                                                      "" 100 LAYER-MODE-MULTIPLY)))
            (set! layer-name "dark shading")
            (set! cutoff (/ cutoff 2))
            ;; (set! angle 110)
            (set! stroke-spacing 1.0)
            <<shading-step>>
        
            (gimp-image-remove-layer image shading-layer-pre)))
    
    This is the `shading-step` routine referenced above which is run twice
    to do the work of overlaying a shading layer over the image.  We find
    the darkest parts of the image using `Threshold` and add diagonal
    lines which look like hatching to the image.  We use `Hurl` and
    `Motion Blur` to generate the hatching lines and then use the
    `Threshold` layer to mask it since we only want the darkest strokes.
    
        (set! dark-layer (car (gimp-layer-copy shading-layer-pre FALSE)))
        (gimp-image-add-layer image dark-layer 0)
        (gimp-drawable-desaturate dark-layer DESATURATE-LUMINANCE)
        (gimp-image-set-active-layer image dark-layer)
        (gimp-drawable-levels hatching-layer HISTOGRAM-VALUE 0.99 1 TRUE 1 0 1 TRUE)
        (gimp-drawable-curves-spline dark-layer HISTOGRAM-VALUE 4 (list->vector (list
                                                                                 (- (* cutoff 0.8) 0.05) 1.0
                                                                                 (+ (* cutoff 0.8) 0.05) 0.0)))
        
        (gimp-selection-all image)
        (gimp-edit-copy dark-layer)
        (gimp-selection-none image)
        
        (gimp-image-add-layer image hatching-layer 0)
        (gimp-image-set-active-layer image hatching-layer)
        (gimp-item-set-name hatching-layer layer-name)
        (gimp-drawable-fill hatching-layer FILL-WHITE)
        (plug-in-randomize-hurl RUN-NONINTERACTIVE image hatching-layer stroke-spacing 1 TRUE (random-next))
        (plug-in-mblur RUN-NONINTERACTIVE image hatching-layer 0 length 135 0 0)
        (gimp-drawable-desaturate hatching-layer DESATURATE-LUMINANCE)
        (gimp-drawable-threshold hatching-layer HISTOGRAM-VALUE 1 1)
        
        (let ((mask (car (gimp-layer-create-mask hatching-layer ADD-MASK-WHITE)))
              (float 0))
          (gimp-layer-add-mask hatching-layer mask)
          (gimp-layer-set-edit-mask hatching-layer TRUE)
          (set! float (car (gimp-edit-paste mask TRUE)))
          (gimp-floating-sel-anchor float))
        
        (gimp-image-remove-layer image dark-layer)
        (gimp-layer-set-mode hatching-layer LAYER-MODE-MULTIPLY)
    
    Here we work on the background layer.
    
    First we convert it to use indexed colors.  This reduces the number of
    colors and results in areas of solid color which look more like an
    illustration than the continuous gradients of a photo.  We'll go into
    details on how we index the colors below.
    
    Next we run a `median-blur` filter to smooth the image.  The strength
    and number of smoothing iterations is controlled by the `Smoothness`
    parameter.  `median-blur` isn't available in GIMP's procedure browser
    so I hacked my version to provide it.
    
    Then we blur the sketch layer to clean up the lines and reduce the
    noise from the photocopy filter.  We do this here and not while we're
    working on the sketch layer because we need to do it while the image
    is indexed.
    
    For the last step here we give the colors a little boost and lighten
    the image.  This isn't necessary but illustrations are often brighter
    and more vivid than reality.  The amount of brightening is controlled
    by the `Lightness` parameter.
    
        (gimp-image-set-active-layer image background-layer)
        <<comic-index>>
        
        (plug-in-median-blur RUN-NONINTERACTIVE image background-layer
                             (+ 1 smoothness (floor (/ (max width height) 800)))
                             50)
        
        (gimp-image-set-active-layer image sketch-layer)
        (plug-in-median-blur RUN-NONINTERACTIVE image sketch-layer 1 50)
        
        (gimp-image-set-active-layer image background-layer)
        (gimp-image-convert-rgb image)
        (when (> lightness 0.0001)
          (gimp-drawable-hue-saturation background-layer HUE-RANGE-ALL 0 0 (+ (* lightness 20) 12) 0))
    
    When we indexed the colors the overlays may have been lightened, but
    we want the overlay lines to be black, so we'll go though and darken
    them here.  This is at the end here because we have to do it after the
    image is converted back to RGB and after the "clean up" blurring while
    the image was indexed.
    
        (gimp-drawable-levels trace-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)
        (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)

4.  index

    This section handles the indexing of the background layer.  Indexing
    an image to flatten the colors works well in some cases, but when
    there are people and faces are small relative to the background often
    the algorithm that chooses colors will pick colors that work well for
    the background but may not be optimal for faces.  The most important
    thing about a comic image is if the people are recognizeable, and
    using sub-optimal colors for skin tones often results in people that
    don't look right.  One way around this is to keep increasing the
    number of colors but this reduces the flattening of the colors, so the
    end result is less cartoon-like.
    
    To get around this we index faces separately from the background, then
    combine the colors found in the two indexing runs.  When combining
    colors we prune background colors that are too close to face colors to
    make it less likely the final indexing run will choose background
    colors for faces.  The graph below shows a run where some colors were
    removed.
    
    ![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_prune.gif)
    
    In the code below, if set up a secondary image for use in the indexing
    runs, which will be described in greater details in the sections below.
    
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
        
              <<get-colormaps>>
        
              <<prune-colors>>
        
              <<build-palette-and-index>>
              ))
    
    First we index only the selected part of the image (allowing up to
    `Face Colors` colors) and save the chosen colors.  Then we index the
    rest of the image (allowing up to `Background Colors` colors) and save
    those.
    
    Indexing an image is destructive so when we index a portion of the
    image just to find out which colors the indexer will choose, we do it
    in a secondary image.
    
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
    
    Here we remove excess colors.  We always add black and white to the
    final palette so we can remove them from the face and background
    colors to prevent duplicates.
    
    If there are background colors which are too close to face colors,
    then in the final indexing run those colors may be used on the face.
    To prevent this, we determine remove any background colors that are
    "too close" to any face colors.  We do this by computing the distance
    between the colors in 3d RGB space.  We define "too close" as being
    less than half the minimum distance between face colors, since our
    intent is that no background colors will get "between" face colors in
    the palette.
    
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
    
    Finally we merge the lists of colors into the final palette and index
    the whole image with it.  While building the palette we add black and
    white and label the colors.
    
    Once we have the palette we can do the final indexing run.
    
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
    
    These are some helper functions used while merging the colors found
    during the two indexing runs to form the final palette.
    
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


<a id="org1800fed"></a>

## Previous Attemps

I made several other attempts before settling on the above technique.
The main ones are listed in this section.


<a id="org23d3bcc"></a>

### Sketch A

Sketch A is based on a tutorial from [Felt Magnet](https://feltmagnet.com/photography/Turning-Photos-Into-Cartoons-A-GIMP-Tutorial).  The method is so
simple it's like magic.  It looks great for some photos but comes out
hollow or too sketchy for others.  It is the only technique I tried
that works well for photos with bad lighting.  Its best results are
for large images when viewed zoomed out.

If you look closely it can be messy.  A side effect of repeating Sobel
is that you get parallel squiggles that appear wormy from close up.

The final script is [here](scripts/sketch-a.scm).

This is an example:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_sketch_a.jpg)

1.  Steps

    -   load an image
    -   sketch layer
        -   new layer is top layer
        -   name it "top"
        -   on top layer
        -   repeat Darken times
            -   Filters > Edge Detect > Edge (Sobel, Black, 1)
        -   set mode DIVIDE


<a id="org0cf0abc"></a>

### Sketch B

Sketch B is based on a tutorial from [gimp.org](https://www.gimp.org/tutorials/Photo_To_Sketch/).  It also uses Sobel to
find edges, but instead of repeating the edge detection to
darken/color the image it equalizes and then masks the sketch layer.
The tutorial applies colors by hand, but I used an indexed version of
the original image as a background color layer so this can run
non-interactively.  This also has a cool artistic look for some photos
when zoomed out but up close it can be messy.

The final script is [here](scripts/sketch-b.scm).

This is an example:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_sketch_b.jpg)

1.  Steps

    -   load an image
        -   name the layer "background"
    -   highpass layer
        -   new layer is top layer
        -   name it "highpass"
        -   Filters > Edge Detect > Sobel
        -   Colors > Auto > Equalize
        -   Colors > Desaturate
        -   duplicate layer
            -   new layer is on top
            -   name it "masked"
            -   Colors > Invert
        -   back to highpass layer
        -   Colors > Curves
            -   zero out the bottom two thirds to clean it up
        -   copy the layer
        -   paste as mask to "masked" layer
            -   right click on "masked" layer, click "Add Layer Mask"
            -   paste copied layer
            -   we no longer need highpass layer, hide it
    -   color layer
        -   go to background layer
        -   Image > Mode > Indexed
        -   Image > Mode > RGB


<a id="org4aef268"></a>

### Comic Book A

This attempt sort of merges both techniques.  It uses two Sobel steps
like Sketch A but then masks them on top of the background layer like
Sketch B.

The final script is [here](scripts/comic-book-a.scm).

This is an example:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_comic_book_a.jpg)

1.  Steps

    -   load an image
        -   name the layer "background"
        -   selective gaussian blur
        -   soft glow
    -   sketch layer
        -   duplicate layer (on top, sketch-mask)
        -   duplicate layer (on-top, sketch-base, active)
        -   Filters > Edge Detect > Edge (2.0)
        -   Filters > Edge Detect > Edge (1.0)
        -   set layer mode DIVIDE
        -   merge down
        -   duplicate layer (on top, mask)
            -   Colors > Invert
            -   Colors > Threshold (0.20)
            -   copy
            -   delete
        -   back to sketch layer
        -   add mask
            -   select mask
            -   paste
            -   anchor
    -   background layer
        -   lighten the colors
            -   Colors > Levels
            -   drag the top right triangle to the left
            -   drag the bottom left triangle to the right
        -   Image > Mode > Indexed (32 colors)
        -   Filters > Blur > Gaussian Blur (4)
        -   Image > Mode > RGB


<a id="org1e7dbb1"></a>

### Comic Book B

The last version struggled with very small images, so this one scales
the image if it is small.  It also uses an Image Gradient edge
detection algorithm instead of Sobel, and only runs it once.  It then
equalizes and desaturates and sets Levels to reduce noise.

The final script is [here](scripts/comic-book-b.scm).

This is an example:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_comic_book_b.jpg)

1.  steps

    -   load an image
        -   scale if too small
        -   soft glow?
    -   sketch layer
        -   duplicate layer (on top, sketch)
        -   Filters > Edge Detect > Image Gradient
        -   Colors > Desaturate > Desaturate
        -   Colors > Levels (reduce noise)
        -   set layer mode SUBTRACTION
    -   background layer
        -   Colors > Brightness Contrast (increase both)
        -   Image > Mode > Indexed (n colors)
        -   Filters > Blur > Selective Gaussian Blur (4, n times)
        -   Image > Mode > RGB
        -   merge visible layers


<a id="org8a190be"></a>

## References

-   [script-fu tutorial](https://docs.gimp.org/en/gimp-using-script-fu-tutorial-script.html)
-   [scheme reference](https://schemers.org/Documents/Standards/R5RS/r5rs.pdf)
-   [GIMP's tinyscheme implementation](https://gitlab.gnome.org/GNOME/gimp/-/blob/master/plug-ins/script-fu/tinyscheme/Manual.txt)


<a id="org5e70abf"></a>

# Literate Programming

This is written as a [literate program](https://en.wikipedia.org/wiki/Literate_programming) using [emacs org-mode](https://orgmode.org/).
[The org file](gimp-comic-book.md) contains the code and
documentation for the comic book filter.  When it is saved, the source
code is generated using `org-babel-tangle` and then copied to GIMP's
scripts directory, and the readme is generated using
`org-md-export-to-markdown`.

    (let ((scripts (org-babel-tangle)))
      (dolist (script scripts)
        (copy-file script (concat (file-name-as-directory scripts-dir) (file-name-nondirectory script)) t)))
    (org-export-to-file 'md "readme.md")

The first line of [the org file](gimp-comic-book.md) configures emacs to run the above
source code block whenever this file is saved, which generates the
scripts and readme.

