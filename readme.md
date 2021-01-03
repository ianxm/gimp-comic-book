# Table of Contents

1.  [Comic Book Filter](#orgda93ed2)
    1.  [Overview](#orgd3d5d63)
    2.  [Example](#orgda56576)
    3.  [Comic Book](#org3af34ea)
        1.  [General Idea](#org5d1b3b0)
        2.  [Steps](#org0d303c3)
        3.  [Script](#org5009c48)
    4.  [Previous Attemps](#orgd94ad0c)
        1.  [Sketch A](#orgef295bb)
        2.  [Sketch B](#org3b5297f)
        3.  [Comic Book A](#orgd5b8348)
        4.  [Comic Book B](#org2bda879)
    5.  [References](#org00490ac)
2.  [Literate Programming](#orgcd20e0e)



<a id="orgda93ed2"></a>

# Comic Book Filter


<a id="orgd3d5d63"></a>

## Overview

This is a GIMP filter to convert an image so that it looks like a
frame from a comic book.  This is similar to to the many cartoon
filters out there.


<a id="orgda56576"></a>

## Example

This is an example photo run through the comic book filter.

Here is the original:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_orig.jpg)

Here is the final result:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_comic.jpg)

These are the GIMP filter settings I used:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_dialog.jpg)

Here are the overlay and background layers that make up the final
result:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_overlays.jpg)
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_background.jpg)


<a id="org3af34ea"></a>

## Comic Book


<a id="org5d1b3b0"></a>

### General Idea

There are two main parts to the comic book image: lines that that
outline shapes and emphasize edges, and colors that fill in the
shapes.

We run two filters to find the lines and overlay them over the
background.  For the background layer we index the colors, then smooth
the image and brighten the colors.

The final script is [here](scripts/comic-book.scm).


<a id="org0d303c3"></a>

### Steps

-   load an image
    -   name the layer "background"
    -   scale if too small
    -   soft glow?
-   trace layer
    -   duplicate layer (on top, trace)
    -   Filters > Edge Detect > Edge (Sobel, 1)
    -   Colors > Desaturate > Desaturate
    -   Colors > Levels
    -   Colors > Invert
    -   set layer mode MULTIPLY
-   sketch layer
    -   duplicate layer (on top, sketch)
    -   Filters > Artistic > Photocopy
    -   set layer mode MULTIPLY
-   background layer
    -   Image > Mode > Indexed (n colors)
    -   Filters > Blur > Selective Gaussian Blur (repeat)
    -   Image > Mode > RGB
    -   Colors > Levels
    -   Colors > Hue Saturation
    -   merge layers


<a id="org5009c48"></a>

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
    boilerblate except four of the parameters, which I'll go over now.

    -   **Smoothness:** a higher value results in more background blurring,
        which looks like smoother curves where indexed colors meet
    -   **Lightness:** a higher value results in lighter colors
    -   **Detail:** a higher value results in more lines
    -   **Fine Detail:** a higher value results in more thin lines
    -   **Allow Resize:** if true, resize the pic to be within bounds that usually work well

    The meaning of the values we set is explained in [section 3.4.8 of the
    GIMP doc](https://docs.gimp.org/2.8/en/gimp-using-script-fu-tutorial-first-script.html).

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

3.  convert

    This is the top-level function for the Comic Book filter.

    We wrap all operations on the image in an undo group so that all of
    the steps show up in GIMP as a single action.  This means that one
    "undo" brings you back to the image before this filter was run.

    Edge detection seems to work best on an image sized in the 2000 to
    4000 range, so we grow or shrink our image if needed (this can be
    disabled with the `Allow Resize` parameter).  If we grew an image
    significantly, we also sharpen it because enlarging an image can cause
    it to blur.

    If we grew it at the beginning we shrink it back to its original size
    at the end.  If we shrunk it at the beginning we leave it since the
    smaller image may be sufficient and we don't want to resize it
    unnecessarily.

    The first operation we do on the image is a `softglow` filter.  It is
    part of ligtening the image so we skip it if the `lightness` parameter
    was set to zero.  The `softglow` helps to reduce lines in the lighter
    parts of a photo (often this reduces line-noise on faces).  The
    `lightness` parameter's range is from `0` to `1` but we scale it to
    the range `0` to `0.2` to keep it to a reasonable amount of
    `softglow`.

    Next we create two layers which overlay the background layer with
    lines, and then work on the background layer.  These steps are covered
    in greater detail in the section below.

    After creating the overlay layers and modifying the background layer
    we merge the layers together.

    Finally we flush the GIMP display to update the image in the buffer.

        (define (script-fu-comic-book image background-layer
                                      colors smoothness lightness
                                      detail fine-detail allow-resize?)
          (gimp-image-undo-group-start image)

          (let* ((width (car (gimp-image-width image)))
                 (height (car (gimp-image-height image)))
                 (min-length 2000)
                 (max-length 4000))

            (when (= allow-resize? TRUE)
              (cond
               ((< height min-length) (gimp-image-scale image (/ (* width min-length) height) min-length))
               ((< width min-length) (gimp-image-scale image min-length (/ (* height min-length) width)))
               ((> height max-length) (gimp-image-scale image (/ (* width max-length) height) max-length))
               ((> width max-length) (gimp-image-scale image max-length (/ (* height max-length) width))))
              (if (< (max (* width 1.5) (* height 1.5)) max-length)
                  (plug-in-unsharp-mask RUN-NONINTERACTIVE image background-layer 3 0.5 0)))

            (if (> lightness 0)
                (plug-in-softglow RUN-NONINTERACTIVE image background-layer 5 (* lightness 0.2) 0.5))

            (let ((sketch-layer (car (gimp-layer-copy background-layer FALSE)))
                  (trace-layer (car (gimp-layer-copy background-layer FALSE))))
              <<trace-layer>>

              <<sketch-layer>>

              <<background-layer>>

              <<darken-overlays>>

              (set! background-layer (car (gimp-image-flatten image))))

            (if (and (= allow-resize? TRUE)
                     (< (max width height) max-length))
                (gimp-image-scale image width height)))

          (gimp-image-undo-group-end image)
          (gimp-displays-flush))

    Here we create a "trace layer" that traces over lines.  It adds thin
    lines wherever there are edges in the image.  The trace layer usually
    picks up some detais that the sketch layer misses.

    We duplicate the background and add the new layer to the top.  The
    main work is done by the Sobel Edge Detection filter, which we run on
    the new layer.  We desaturate to convert to greyscale since we don't
    want color info and adjust levels in the trace layer to stengthen the
    most significant lines and dim the noise.  We use the `Fine Detail`
    parameter to control this adjustment.  If `Fine Detail` is turned down
    to zero, we skip this step entirely.npn

    Finally we invert the trace layer and set its mode to `MULTIPLY` so
    that the lines show up overlayed on the background.

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

    Here we create a layer that outlines shapes, which we will call the
    sketch layer.  First we create and add the new layer on top of the
    background layer.

    Next we use the `photocopy` filter to convert the layer into lines
    where there the image is darkest.  We use the `Detail` parameter to
    determine how sensitive photocopy should be.  This does a good job of
    marking edges, but also results in noise in large dark areas.  To
    reduce that effect we lighten the image before the `photocopy` call
    and darken it back after.  If `Detail` is turned down to zero we skip
    this step entirely.

    The `photocopy` filter produces an inverted greyscale image so there's
    no need to desaturate or invert the sketch layer.  We just set its
    mode to `MULTIPLY` and are done here.

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

    Here we work on the background layer.

    First we convert it to use indexed colors.  This reduces the number of
    colors and results in areas of solid color which look more like
    an illustration than the continuous gradients of a photo.

    Next we run a blur filter to smooth the image.  The number of
    smoothing iterations is controlled by the `Smoothness` parameter.  If
    the gaussian filter was run on the RGB image, it would make it blurry,
    but on an indexed image it smoothes out curves and eliminates some
    noise.

    Then we blur the sketch layer to clean up the lines and reduce the
    noise from the photocopy filter.  We do this here and not while we're
    working on the sketch layer because we need to do it while the image
    is indexed.  I wanted to use the gaussian blur but couldn't get it to
    work so I used a selective gaussian blur with a high threshold, which
    should be equivalent but slower.

    Actually I'd rather use median blur than gauss for all three blurring
    runs but GIMP currently doesn't provide a way to run median blur from
    script-fu.

    For the last step here we give the colors a boost and lighten the
    image.  Note that the order of those steps matters.  This isn't
    necessary but illustrations are often brighter and more vivid than
    reality.  The amount of brightening is controlled by the `Lightness`
    parameter.

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

    When we indexed the colors the overlays may have been lightened, but
    we want them to be black, so we'll go though and darken them here.

        (gimp-image-set-active-layer image trace-layer)
        (gimp-drawable-levels trace-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)
        (gimp-image-set-active-layer image sketch-layer)
        (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)


<a id="orgd94ad0c"></a>

## Previous Attemps

I made several other attempts before settling on the above technique.
The main ones are listed in this section.


<a id="orgef295bb"></a>

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


<a id="org3b5297f"></a>

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


<a id="orgd5b8348"></a>

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


<a id="org2bda879"></a>

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


<a id="org00490ac"></a>

## References

-   [script-fu tutorial](https://docs.gimp.org/en/gimp-using-script-fu-tutorial-script.html)
-   [scheme reference](https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_toc.html)


<a id="orgcd20e0e"></a>

# Literate Programming

This is written as a [literate program](https://en.wikipedia.org/wiki/Literate_programming) using [emacs org-mode](https://orgmode.org/).  [The org
file](gimp-comic-book.md) contains the code and documentation for the comic book filter.
When it is saved, the source code is generated using
`org-babel-tangle` and then copied to GIMP's scripts directory, and
the readme is generated using `org-md-export-to-pemarkdown`.

    (let ((scripts (org-babel-tangle)))
      (dolist (script scripts)
        (copy-file script (concat (file-name-as-directory scripts-dir) (file-name-nondirectory script)) t)))
    (org-export-to-file 'md "readme.md")

The first line of [the org file](gimp-comic-book.md) configures emacs to run the `generate`
source code block whenever this file is saved, which generates the
scripts and readme.  See [Literate Programming](#orgcd20e0e) for more details.
