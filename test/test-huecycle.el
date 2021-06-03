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
           (huecycle--default-start-color ,huecycle--default-start-color)
           )
       (cl-flet ((huecycle--input-pending () ,input-pending-func-form))
         (with-temp-buffer
           ,body)))))


;; Generaly huecycle should
;; - update progress correctly
;; - set faces correctly
;; - cleanup properly
;; TODO huecycle-set-faces sets correctly
;; - maps input call to list of interp datum correctly

;; ---------- Generj
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

(ert-deftest huecycle-cleans-up-persist ()
  "Test huecycle cleans up properly when persist is set."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (print face-remapping-alist)
     (should (= 1 (length face-remapping-alist))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       '((((foreground . default)) :persist t)))
   :huecycle-iterations 3))
