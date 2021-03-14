
# Table of Contents

1.  [Comic Book Filter](#org02fcac3)
    1.  [Overview](#org264f151)
    2.  [Example](#orga06dfa3)
    3.  [Comic Book](#orga6692a4)
        1.  [General Idea](#orgf8c0456)
        2.  [Steps](#org17eeda4)
        3.  [Script](#org7d176ad)
    4.  [Previous Attemps](#orgbff6c90)
        1.  [Sketch A](#org0b8c108)
        2.  [Sketch B](#org8ceeb3d)
        3.  [Comic Book A](#orgc9c4be7)
        4.  [Comic Book B](#org391057f)
    5.  [References](#org3419916)
2.  [Literate Programming](#orgd599cd5)



<a id="org02fcac3"></a>

# Comic Book Filter


<a id="org264f151"></a>

## Overview

This is a GIMP filter to convert an image so that it looks like a
frame from a comic book.  This is similar to the many cartoon filters
out there.


<a id="orga06dfa3"></a>

## Example

This is an example photo run through the comic book filter.

Here is the original:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_orig.jpg)

Here is the final result:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_comic.jpg)

These are the GIMP filter settings I used to convert this image:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_dialog.jpg)

In case you're curious, here are the overlay and background layers
that make up the final result:
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_overlays.jpg)
![img](https://ianxm-githubfiles.s3.amazonaws.com/gimp-comic-book/utah_background.jpg)


<a id="orga6692a4"></a>

## Comic Book


<a id="orgf8c0456"></a>

### General Idea

There are two main parts to the comic book image: lines that that
outline shapes and emphasize edges, and colors that fill in the
shapes.

We run two filters to find the lines and overlay them over the
background.  For the background layer we index the colors, then smooth
the image and brighten the colors.

The final script is [here](scripts/comic-book.scm).


<a id="org17eeda4"></a>

### Steps

-   load an image
    -   scale if too small, sharpen if significantly scaled
    -   Colors > Curves (lighten)
    -   Fitlers > Artistic > Softglow
-   trace layer (fine detail)
    -   duplicate layer (on top, trace)
    -   Filters > Edge Detect > Edge (Sobel, 1)
    -   Colors > Desaturate > Desaturate
    -   Colors > Levels
    -   Colors > Invert
    -   set layer mode MULTIPLY
-   sketch layer (detail)
    -   duplicate layer (on top, sketch)
    -   Filters > Artistic > Photocopy
    -   set layer mode MULTIPLY
-   background layer (colors)
    -   Image > Mode > Indexed (n colors)
    -   Filters > Blur > Selective Gaussian Blur (repeat)
    -   Image > Mode > RGB
    -   Colors > Levels
    -   Colors > Hue Saturation
    -   merge layers


<a id="org7d176ad"></a>

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
    boilerblate except five of the parameters, which I'll go over now.
    
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
         "RGB* GRAY*"                             ; image type that the script works on
         SF-IMAGE      "Image"      0             ; the image
         SF-DRAWABLE   "Drawable"   0             ; the layer
         SF-ADJUSTMENT "Colors"           '(64 3 128 1 10 0 0)
         SF-ADJUSTMENT "Smoothness"       '(2 0 5 1 1 0 1)
         SF-ADJUSTMENT "Lightness"        '(0.1 0 1 0.1 0.2 2 0)
         SF-ADJUSTMENT "Detail"           '(0.5 0 1 0.1 0.2 2 0)
         SF-ADJUSTMENT "Fine Detail"      '(0.5 0 1 0.1 0.2 2 0)
         SF-TOGGLE     "Allow Resize"     TRUE)
        (script-fu-menu-register "script-fu-comic-book" "<Image>/Filters/Artistic")

3.  convert

    This is the top-level function for the Comic Book filter.
    
    We wrap all operations on the image in an undo group so that all of
    the steps show up in GIMP as a single action.  This means that one
    "undo" brings you back to the image before this filter was run.
    
    Edge detection seems to work best on an image sized in the 1200 to
    4000 range, so we enlarge or shrink our image if needed (this can be
    disabled with the `Allow Resize` parameter).  If we enlarge an image
    significantly, we also sharpen it because enlarging an image can cause
    it to blur.
    
    If we enlarge it at the beginning we shrink it back to its original
    size at the end.  If we shrunk it at the beginning we leave it since
    the smaller image may be sufficient and we don't want to resize it
    unnecessarily.
    
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
                                      colors smoothness lightness
                                      detail fine-detail allow-resize?)
          (gimp-image-undo-group-start image)
        
          (let* ((width (car (gimp-image-width image)))
                 (height (car (gimp-image-height image)))
                 (min-length 1200)
                 (max-length 4000)
                 (sf 1))
        
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
              <<trace-layer>>
        
              <<sketch-layer>>
        
              <<background-layer>>
        
              <<darken-overlays>>
        
              (set! background-layer (car (gimp-image-flatten image))))
        
            (if (and (= allow-resize? TRUE)
                     (< (max width height) min-length))
                (gimp-image-scale image width height)))
        
          (gimp-image-undo-group-end image)
          (gimp-displays-flush))
    
    Here we create a "trace layer" that traces over lines.  It adds thin
    lines wherever there are edges in the image.  The trace layer usually
    picks up some details that the sketch layer misses.
    
    We duplicate the background and add the new layer to the top.  We
    lighten the new layer with `curves` to wash out any glare or shiny
    spots so they aren't picked up by the edge detection.  The main work
    is done by the Sobel Edge Detection filter, which we run on the new
    layer.  We desaturate to convert to greyscale since we don't want
    color info.  Then we adjust levels in the trace layer to stengthen the
    most significant lines and dim the noise.  We use the `Fine Detail`
    parameter to control this adjustment.  If `Fine Detail` is turned down
    to zero, we skip this step entirely.
    
    Finally we invert the trace layer and set its mode to `MULTIPLY` so
    that the lines show up overlayed on the background.
    
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
        
          (let ((count 0))
            (while (< count 2)
                   (plug-in-unsharp-mask RUN-NONINTERACTIVE image sketch-layer 2 0.5 0)
                   (set! count (+ count 1))))
        
          (gimp-layer-set-mode sketch-layer LAYER-MODE-MULTIPLY))
    
    Here we work on the background layer.
    
    First we convert it to use indexed colors.  This reduces the number of
    colors and results in areas of solid color which look more like
    an illustration than the continuous gradients of a photo.
    
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
        (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE colors FALSE TRUE "")
        
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
    
    When we indexed the colors the overlays may have been lightened, but
    we want the overlay lines to be black, so we'll go though and darken
    them here.  This is at the end here because we have to do it after the
    image is converted back to RGB and after the "clean up" blurring while
    the image was indexed.
    
        (gimp-drawable-levels trace-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)
        (gimp-drawable-levels sketch-layer HISTOGRAM-VALUE 0.4 1 TRUE 1 0 1 TRUE)


<a id="orgbff6c90"></a>

## Previous Attemps

I made several other attempts before settling on the above technique.
The main ones are listed in this section.


<a id="org0b8c108"></a>

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


<a id="org8ceeb3d"></a>

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


<a id="orgc9c4be7"></a>

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


<a id="org391057f"></a>

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


<a id="org3419916"></a>

## References

-   [script-fu tutorial](https://docs.gimp.org/en/gimp-using-script-fu-tutorial-script.html)
-   [scheme reference](https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_toc.html)
-   [scheme implementation](https://gitlab.gnome.org/GNOME/gimp/-/tree/master/plug-ins/script-fu/tinyscheme)


<a id="orgd599cd5"></a>

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

