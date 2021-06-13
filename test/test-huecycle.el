;; test-huecycle.el --- Tests for huecycle -*- no-byte-compile: t; -*-

(require 'huecycle)
(require 'ert)

(cl-defmacro huecycle-with-test-env
    (body &key
          (huecycle-iterations 1)
          (huecycle-cycle-duration 0)
          (huecycle-step-size 0.033333)
          (huecycle--interpolate-data '())
          (huecycle--buffer-data '())
          (huecycle--active-buffers '())
          (huecycle--max-active-buffers 10)
          (huecycle--idle-timer nil)
          (huecycle--current-time 0)
          (huecycle--default-start-color "#888888"))
  "Set up testing enviroment for huecycle and run BODY.
BODY is a single lisp form, to run multiple statements use `progn'. HUECYCLE-ITERATIONS controls how many loop
iterations `huecycle' does. If it is <= 0, it will loop forever."
  (let ((input-pending-func-form
         (if (<= huecycle-iterations 0)
             'nil
           `(if (>= huecycle-counter ,huecycle-iterations)
                t
              (progn
                (setq huecycle-counter (1+ huecycle-counter))
                nil)))))
    `(let ((huecycle-counter 0)
           (huecycle-iterations ,huecycle-iterations)
           (huecycle-cycle-duration ,huecycle-cycle-duration)
           (huecycle-step-size ,huecycle-step-size)
           (huecycle--interpolate-data ,huecycle--interpolate-data)
           (huecycle--buffer-data ,huecycle--buffer-data)
           (huecycle--active-buffers ,huecycle--active-buffers)
           (huecycle--max-active-buffers ,huecycle--max-active-buffers)
           (huecycle--idle-timer ,huecycle--idle-timer)
           (huecycle--current-time ,huecycle--current-time)
           (huecycle--default-start-color ,huecycle--default-start-color))
       (cl-flet ((huecycle--input-pending () ,input-pending-func-form))
         (with-temp-buffer
           ,body)))))


(defun huecycle-get-face-color (spec face)
  "Return hex string of the color of FACE based on SPEC from `face-remapping-alist'."
  (let ((remaps
         (seq-some
          (lambda (remaps) (if (equalp face (nth 0 remaps)) remaps))
          face-remapping-alist)))
    (seq-some (lambda (item) (if (and (listp item) (equalp spec (nth 0 item))) (nth 1 item)))
              remaps)))

;; ===== T E S T S ====================================================================================

(ert-deftest huecycle-test-huecycle-runs ()
  "Test that huecycle runs without errors."
   (huecycle-with-test-env
        (huecycle)))

(ert-deftest huecycle-cleans-up ()
  "Test huecycle cleans up after itself."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))))

(ert-deftest huecycle-cleans-up-many-iterations ()
  "Test huecycle cleans up after itself with many iterations."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle-iterations 100))

(ert-deftest huecycle-cleans-up-with-duration ()
  "Test huecycle cleans up after itself when ending `huecycle-cycle-duration' is positive."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle-cycle-duration 1
   :huecycle-iterations 0))

(ert-deftest huecycle-cleans-up-multiple-faces ()
  "Test huecycle cleans up multiple faces when finishing."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum
                                       '(((foreground . default))
                                         ((background . highlight))
                                         ((distant-foreground . font-lock-string-face))))
   :huecycle-iterations 5))

(ert-deftest huecycle-cleans-up-multiple-faces-2 ()
  "Test huecycle cleans up multiple faces when finishing."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum
                                       '(((foreground . (default highlight)))
                                         ((foreground . (font-lock-comment-face)))
                                         ((background . (highlight font-lock-keyword-face)))
                                         ((background . (highlight font-lock-string-face)))
                                         ((distant-foreground . font-lock-string-face))))
   :huecycle-iterations 5))

(ert-deftest huecycle-cleans-up-multiple-faces-3 ()
  "Test huecycle cleans up multiple faces when finishing."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum
                                       '(((foreground . (highlight font-lock-constant-face)))
                                         ((foreground . (default font-lock-keyword-face)))
                                         ((background . (font-lock-keyword-face font-lock-builtin-face)))
                                         ((background . (highlight font-lock-warning-face font-lock-string-face)))
                                         ((distant-foreground . (font-lock-string-face default)))))
   :huecycle-iterations 5))

(ert-deftest huecycle-cleans-up-multiple-configs-clean-up ()
  "Test huecycle cleans up properly when various configs are set."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       '((((foreground . default)) :speed 2.0)
                                         (((background . highlight)) :speed 3.0 :random-color-hue-range (0 0.5))))
   :huecycle-iterations 3))

(ert-deftest huecycle-cleans-up-persist ()
  "Test huecycle cleans up properly when persist is set."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (= 1 (length face-remapping-alist))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       '((((foreground . default)) :persist t)))
   :huecycle-iterations 3))

(ert-deftest huecycle-uses-next-colors ()
  "Test huecycle interpolate toward next color."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp "#ffffff" (huecycle-get-face-color :foreground 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((foreground . default))
                                          :persist t
                                          :interp-func huecycle-interpolate-step
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#ffffff" "#ff0000"))))
   :huecycle-step-size 0.25
   :huecycle-iterations 3))

(ert-deftest huecycle-uses-next-colors-2 ()
  "Test huecycle interpolate toward next color."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp "#000004" (huecycle-get-face-color :background 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((background . default))
                                          :persist t
                                          :interp-func huecycle-interpolate-step
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 0.25
   :huecycle-iterations 22))

(ert-deftest huecycle-uses-next-colors-3 ()
  "Test huecycle interpolate toward next color."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp "#000004" (huecycle-get-face-color :background 'default)))
     (should (equalp "#000004" (huecycle-get-face-color :foreground 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((background . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))
                                         (((foreground . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 0.5
   :huecycle-iterations 7))

(ert-deftest huecycle-uses-next-colors-4 ()
  "Test huecycle interpolate toward next color."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equalp "#000002" (huecycle-get-face-color :background 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((background . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 0.5
   :huecycle-iterations 3))

(ert-deftest huecycle-uses-random-list-colors ()
  "Test huecycle interpolates to color in list")

;; TODO fix get-next
;; (defun sample ()
;;   (huecycle-with-test-env
;;    (progn
;;      (huecycle)
;;      (huecycle--interp-datum-color-list-index (nth 0 huecycle--buffer-data))
;;      ;; (should (equalp "#000002" (huecycle-get-face-color :background 'default)))
;;      )
;;    :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
;;                                        `((((background . default))
;;                                           :persist t
;;                                           :next-color-func huecycle-get-next-list-color
;;                                           :color-list ("#000001" "#000002" "#000003" "#000004"))))
;;    :huecycle-step-size 0.5
;;    :huecycle-iterations 1))
