;;; local_packages/colorize-hl-line/colorize-hl-line.el -*- lexical-binding: t; -*-

;; DONE move from test.el
;; DONE cleanup naming conventions
;; DONE support oscillation
;; <NOT NEEDED> break up coloriz-hl-line function to subfunctions for advising purposes
;; - tick
;; - setting color
;; - getting next color
;; - cleanup
;; DONE make public interp function
;; - change things to defvar for documentation
;; DONE leave-me-alone command
;; DONE set idle timer command
;; TODO config to colorize highlight as well
;; - add list of faces that also get colorized -> (or change it to list all together)
;; TODO config to disable revertion when moving cursor
;; TODO rename package to "colorize" (or something to do with lerping colors over time idly)

(eval-when-compile (require 'cl-lib))


(defvar colorize-hl-line-next-color-function #'colorize-hl-line--get-random-hsl-color
  "Function used to generate next color to interpolate to")

(defvar colorize-hl-line-interp-function #'colorize-hl-line-interpolate-linear
  "Function used to interpolate 2 colors")

(defvar colorize-hl-line-colors-list nil
  "Colors used to interpolate between in order if `colorize-hl-line--get-next-hsl-color' is used")

(defvar colorize-hl-line--idle-timer nil
  "Idle timer used to colorize")

(defvar colorize-hl-line-faces '(hl-line)
  "List of faces to change color")

(defvar colorize-hl-line--cookies nil
  "List of cookies used for face remaps, or nil")

(defun colorize-hl-line-random-next ()
  "Makes hl-line interpolate to a random color each cycle"
  (setq colorize-hl-line-next-color-function #'colorize-hl-line--get-random-hsl-color))

(defun colorize-hl-line-list-next (colors)
  "Makes hl-line interpolate to each color in `colors' in order.
`colors' is a list of hex strings with 2 digits per component"
  (setq colorize-hl-line-next-color-function (apply-partially #'colorize-hl-line--get-next-hsl-color
                                                              (lambda ()
                                                                (mod
                                                                 (1+ colorize-hl-line--colors-list-index)
                                                                 (length colorize-hl-line-colors-list)))))
  (setq colorize-hl-line-colors-list (mapcar #'colorize-hl-line--hex-to-hsl-color colors)))

(defun colorize-hl-line-list-random (colors)
  "Makes hl-line interpolate to colors in `colors' in a random order.
`colors' is a list of hex strings with 2 digits per component"
  (setq colorize-hl-line-next-color-function (apply-partially #'colorize-hl-line--get-next-hsl-color
                                                              (lambda () (random (length colorize-hl-line-colors-list)))))
  (setq colorize-hl-line-colors-list (mapcar #'colorize-hl-line--hex-to-hsl-color colors)))

(cl-defstruct (colorize-hl-line--color (:constructor colorize-hl-line--color-create)
                                 (:copier nil))
  hue saturation luminance)


(defun colorize-hl-line--hex-to-rgb (hex)
  "Converts hex string (2 digits per component) to rgb tuple"
  (cl-assert (length= hex 7) "hex string should have 2 digits per component")
  (let (
        (red
         (/ (string-to-number (substring hex 1 3) 16) 255.0))
        (green
         (/ (string-to-number (substring hex 3 5) 16) 255.0))
        (blue
         (/ (string-to-number (substring hex 5 7) 16) 255.0)))
    (list red green blue)))

(defun colorize-hl-line--hex-to-hsl-color (color)
  "Converts hex string `color' to a `colorize-hl-line--color'"
  (pcase (apply 'color-rgb-to-hsl (colorize-hl-line--hex-to-rgb color))
    (`(,hue ,sat ,lum) (colorize-hl-line--color-create :hue hue :saturation sat :luminance lum))
    (`(,_) error "Could not parse hl-line")))

(defun colorize-hl-line--get-start-hl-line-color ()
  "Returns the current background color of the hl-line as `colorize-hl-line--color'"
  (let ((hsl (apply 'color-rgb-to-hsl (colorize-hl-line--hex-to-rgb (face-attribute 'hl-line :background)))))
    (pcase hsl
      (`(,hue ,sat ,lum) (colorize-hl-line--color-create :hue hue :saturation sat :luminance lum))
      (`(,_) error "Could not parse hl-line"))))


;; TODO setters for this that take in 2 args
(setq colorize-hl-line--hue-low 0.0)
(setq colorize-hl-line--hue-high 1.0)
(setq colorize-hl-line--saturation-low 0.5)
(setq colorize-hl-line--saturation-high 1.0)
(setq colorize-hl-line--luminance-low 0.2)
(setq colorize-hl-line--luminance-high 0.3)

(defun colorize-hl-line--get-random-hsl-color ()
  "Returns random `hl-line-hsl-color'"
  (colorize-hl-line--color-create
   :hue (colorize-hl-line--get-random-float-from colorize-hl-line--hue-low colorize-hl-line--hue-high)
   :saturation (colorize-hl-line--get-random-float-from colorize-hl-line--saturation-low colorize-hl-line--saturation-high)
   :luminance (colorize-hl-line--get-random-float-from colorize-hl-line--luminance-low colorize-hl-line--luminance-high)))

(defun colorize-hl-line--get-random-float-from (lower upper)
  "Gets random float from in range [lower, upper].
`lower' and `upper' should be in range [0.0, 1.0]"
  (cl-assert (and (>= lower 0.0) (<= lower 1.0)) "lower is not in range [0, 1]")
  (cl-assert (and (>= upper 0.0) (<= upper 1.0)) "upper is not in range [0, 1]")
  (cl-assert (<= lower upper) "lower should be <= upper")
  (let* (
         (high-number 10000000000)
         (lower-int (truncate (* lower high-number)))
         (upper-int (truncate (* upper high-number))))
    (/ (* 1.0 (+ lower-int (random (- upper-int lower-int)))) high-number)))

(defun colorize-hl-line--get-next-hsl-color (next-index-func)
  "Returns next color from `colorize-hl-line-colors-list'. If it is nil or empty, returns `nil'
`next-index-func' is called to compute the next index for the color next to be determined"
  (if (length= colorize-hl-line-colors-list 0)
      nil
    (if (or (not (boundp 'colorize-hl-line--colors-list-index)))
        (setq colorize-hl-line--colors-list-index 0)
     ;;(setq colorize-hl-line--colors-list-index (mod (1+ colorize-hl-line--colors-list-index) (length colorize-hl-line-colors-list)))
     (setq colorize-hl-line--colors-list-index (funcall next-index-func))
     (nth colorize-hl-line--colors-list-index colorize-hl-line-colors-list))))

(defun colorize-hl-line--clamp (value low high)
  "Clamps `value' between `low' and `high'"
  (max (min value high) low))

(defun colorize-hl-line-interpolate-linear (progress start end)
  "Returns new color that is the result of interplating the colors of `start' and `end' linearly.
`progress' is a float in the range [0, 1], but providing a value outside of that will extrapolate new values.
`start' and `end' are `colorize-hl-line--color'"
  (let (
        (new-hue
         (colorize-hl-line--clamp
          (+ (* (- 1 progress) (colorize-hl-line--color-hue start)) (* progress (colorize-hl-line--color-hue end))) 0 1))
        (new-sat
         (colorize-hl-line--clamp
          (+ (* (- 1 progress) (colorize-hl-line--color-saturation start)) (* progress (colorize-hl-line--color-saturation end))) 0 1))
        (new-lum
         (colorize-hl-line--clamp
          (+ (* (- 1 progress) (colorize-hl-line--color-luminance start)) (* progress (colorize-hl-line--color-luminance end))) 0 1)))
    (colorize-hl-line--color-create :hue new-hue :saturation new-sat :luminance new-lum)))

(defun colorize-hl-line--hsl-color-to-hex (hsl-color)
  "Converts `colorize-hl-line--color' to hex string with 2 digits for each component"
  (let ((rgb (color-hsl-to-rgb
              (colorize-hl-line--color-hue hsl-color)
              (colorize-hl-line--color-saturation hsl-color)
              (colorize-hl-line--color-luminance hsl-color))))
    (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 2)))

;; *colorize hl-line*
;; --- STARTUP
;; init starting color to current hl-line
;; init target color to random color
;; init current cookie to nil
;; init progress to 0.0
;; --- MAIN
;; while no input is pending...
;; - update progress by `deltatime'
;; - remove last cookie
;; - setq cookie to new color (linear interp)
;; if sample >= 1.0
;; + reset sample to 0.0
;; + set start-color to target-colo
;; + set target-color to a random color
;; --- CLEANUP
;; - face-remap remove the cookie

(setq colorize-hl-line-step-size 0.01)
(setq colorize-hl-line-step-multiple 1.0)

(defun colorize-hl-line ()
  "Let the `hl-line`'s background change color"
  (interactive)
  ;; startup
  (let
        ((start-color (colorize-hl-line--get-start-hl-line-color))
         (target-color (colorize-hl-line--get-random-hsl-color))
         (progress 0.0)
         (next-color-func (if colorize-hl-line-next-color-function
                              colorize-hl-line-next-color-function #'colorize-hl-line--get-random-hsl-color))
         (interp-func (if colorize-hl-line-interp-function colorize-hl-line-interp-function #'colorize-hl-line-interpolate-linear)))
      ;; main loop
      (while (not (input-pending-p))
        (sit-for colorize-hl-line-step-size)

        (setq progress (+ progress (* colorize-hl-line-step-size colorize-hl-line-step-multiple)))

        (colorize-hl-line--remove-cookies)
        (colorize-hl-line--set-faces-backgrounds
         (funcall interp-func progress start-color target-color))
        (colorize-hl-line--redisplay-faces)

        (if (>= progress 1.0)
            (progn
              (setq progress 0.0)
              (setq start-color target-color)
              (setq target-color (funcall next-color-func)))))

      ;; cleanup face-remapping
      (colorize-hl-line--remove-cookies)))

(defun colorize-hl-line--remove-cookies ()
  "Remove all face cookies, and set `colorize-hl-line--cookies' to '()"
  (if (and colorize-hl-line--cookies (listp colorize-hl-line--cookies))
      (progn
        (dolist (cookie colorize-hl-line--cookies)
               (face-remap-remove-relative cookie))
        (setq colorize-hl-line--cookies '()))))
      
(defun colorize-hl-line--set-faces-backgrounds (new-color)
  "Sets all faces in `colorize-hl-line-faces' background color ot `new-color', and stores resulting cookies in
`colorize-hl-line--cookies'"
  (setq colorize-hl-line--cookies
        (mapcar (lambda (face) (face-remap-add-relative
                                face
                                :background (colorize-hl-line--hsl-color-to-hex new-color)))
                colorize-hl-line-faces)))

(defun colorize-hl-line--redisplay-faces ()
  "Redisplays each face in the current frame"
  (dolist (face colorize-hl-line-faces)
    (face-spec-recalc face (selected-frame))))

(setq colorize-hl-line-faces '(hl-line mode-line))
;; (colorize-hl-line--set-faces-backgrounds "#880000")
;; (colorize-hl-line--remove-cookies)

;; (colorize-when-idle 1)


(defun colorize-stop-idle ()
  "Stops the colorization effect when idle"
  (interactive)
  (cancel-timer colorize-hl-line--idle-timer)
  (setq colorize-hl-line--idle-timer nil))

(defun colorize-when-idle (secs)
  "Starts the colorization effect. when idle for `secs' seconds"
  (setq colorize-hl-line--idle-timer (run-with-idle-timer secs t 'colorize-hl-line)))

;; TESTING ========================================
