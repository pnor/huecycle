;; test-huecycle.el --- Tests for huecycle -*- no-byte-compile: t; -*-

(require 'huecycle)
(require 'ert)

(defun huecycle--test-input-pending (huecycle-iterations)
  "Function used in testing enviroments to replace `huecycle--input-pending'.
HUECYCLE-ITERATIONS is the number of times `huecycle' should loop for.
Assumes testing enviroment has defined `huecycle-counter' to track current iteration number."
  (unless (<= huecycle-iterations 0)
    (if (>= huecycle-counter huecycle-iterations)
        t
      (progn
        (setq huecycle-counter (1+ huecycle-counter))
        nil))))

(defun huecycle--test-sit-for (_)
  "Function used in testing enviroments to replace `sit-for'."
  t)

(defun huecycle-get-face-color (spec face)
  "Return hex string of the color of FACE based on SPEC from `face-remapping-alist'."
  (let ((remaps
         (seq-some
          (lambda (remaps) (if (equal face (nth 0 remaps)) remaps))
          face-remapping-alist)))
    (seq-some (lambda (item) (if (and (listp item) (equal spec (nth 0 item))) (nth 1 item)))
              remaps)))

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
  `(let ((face-remapping-alist '())
         (huecycle-counter 0)
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
     (cl-letf
         (((symbol-function #'huecycle--input-pending) (apply-partially #'huecycle--test-input-pending huecycle-iterations))
          ((symbol-function #'sit-for) #'huecycle--test-sit-for))
       (with-temp-buffer
         ,body))))

;; ===== T E S T S ====================================================================================

(ert-deftest huecycle-test-huecycle-runs ()
  "Test that huecycle runs without errors."
  (huecycle-with-test-env
   (huecycle)))

(ert-deftest huecycle-cleans-up ()
  "Test huecycle cleans up after itself."
  ;; Simple case
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default)))))
  ;; Many iterations
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle-iterations 100)
  ;; Test huecycle cleans up after itself when ending `huecycle-cycle-duration' is positive.
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle-cycle-duration 1
   :huecycle-iterations 0)
  ;; Test huecycle cleans up multiple faces when finishing."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum
                                       '(((foreground . default))
                                         ((background . highlight))
                                         ((distant-foreground . font-lock-string-face))))
   :huecycle-iterations 5)
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum
                                       '(((foreground . (default highlight)))
                                         ((foreground . (font-lock-comment-face)))
                                         ((background . (highlight font-lock-keyword-face)))
                                         ((background . (highlight font-lock-string-face)))
                                         ((distant-foreground . font-lock-string-face))))
   :huecycle-iterations 5)
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum
                                       '(((foreground . (highlight font-lock-constant-face)))
                                         ((foreground . (default font-lock-keyword-face)))
                                         ((background . (font-lock-keyword-face font-lock-builtin-face)))
                                         ((background . (highlight font-lock-warning-face font-lock-string-face)))
                                         ((distant-foreground . (font-lock-string-face default)))))
   :huecycle-iterations 5)
  ;; Test huecycle cleans up properly when various configs are set.
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal '() face-remapping-alist)))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       '((((foreground . default)) :speed 2.0)
                                         (((background . highlight)) :speed 3.0 :random-color-hue-range (0 0.5))))
   :huecycle-iterations 3))

(ert-deftest huecycle-persists ()
  "Test whether setting persist works."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (= 1 (length face-remapping-alist))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       '((((foreground . default)) :persist t)))
   :huecycle-iterations 3))

(ert-deftest huecycle-uses-next-colors ()
  "Test huecycle interpolate toward next color, and in right order."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal "#ffffff" (huecycle-get-face-color :foreground 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((foreground . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#ffffff" "#ff0000"))))
   :huecycle-step-size 1
   :huecycle-iterations 1)
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal "#000004" (huecycle-get-face-color :background 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((background . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 1
   :huecycle-iterations 4)
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal "#000004" (huecycle-get-face-color :background 'default)))
     (should (equal "#000004" (huecycle-get-face-color :foreground 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((background . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))
                                         (((foreground . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 1
   :huecycle-iterations 4)
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal "#000003" (huecycle-get-face-color :background 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((background . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 1
   :huecycle-iterations 7))

(ert-deftest huecycle-uses-random-colors ()
  "Test huecycle interpolate towards a random color."
  (let ((test-colors-list '("#000001" "#000002" "#000003" "#000004")))
    (huecycle-with-test-env
     (progn
       (huecycle)
       (should (member (huecycle-get-face-color :foreground 'default) test-colors-list)))
     :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                         `((((foreground . default))
                                            :persist t
                                            :next-color-func huecycle-get-random-color-from-list
                                            :color-list ,test-colors-list)))
     :huecycle-step-size 1
     :huecycle-iterations 1))
  (let ((test-colors-list '("#000001")))
    (huecycle-with-test-env
     (progn
       (huecycle)
       (should (member (huecycle-get-face-color :foreground 'default) test-colors-list)))
     :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                         `((((foreground . default))
                                            :persist t
                                            :next-color-func huecycle-get-random-color-from-list
                                            :color-list ,test-colors-list)))
     :huecycle-step-size 1
     :huecycle-iterations 1))
  (let ((test-colors-list '("#000001" "#000002" "#000003" "#000004" "#000005")))
    (huecycle-with-test-env
     (progn
       (huecycle)
       (should (member (huecycle-get-face-color :foreground 'default) test-colors-list)))
     :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                         `((((foreground . default))
                                            :persist t
                                            :next-color-func huecycle-get-random-color-from-list
                                            :color-list ,test-colors-list)))
     :huecycle-step-size 1
     :huecycle-iterations 5))
  (let ((test-colors-list '("#000001" "#000002" "#000003" "#000004" "#000005" "#000006")))
    (huecycle-with-test-env
     (progn
       (huecycle)
       (should (member (huecycle-get-face-color :foreground 'highlight) test-colors-list))
       (should (member (huecycle-get-face-color :foreground 'font-lock-constant-face) test-colors-list))
       (should (member (huecycle-get-face-color :foreground 'default) test-colors-list))
       (should (member (huecycle-get-face-color :foreground 'font-lock-keyword-face) test-colors-list))
       (should (member (huecycle-get-face-color :background 'font-lock-keyword-face) test-colors-list))
       (should (member (huecycle-get-face-color :background 'font-lock-builtin-face) test-colors-list))
       (should (member (huecycle-get-face-color :background 'highlight) test-colors-list))
       (should (member (huecycle-get-face-color :background 'font-lock-warning-face) test-colors-list))
       (should (member (huecycle-get-face-color :background 'font-lock-string-face) test-colors-list))
       (should (member (huecycle-get-face-color :distant-foreground 'font-lock-string-face) test-colors-list))
       (should (member (huecycle-get-face-color :distant-foreground 'default) test-colors-list)))
     :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                         `((((foreground . (highlight font-lock-constant-face)))
                                           :persist t
                                           :next-color-func huecycle-get-random-color-from-list
                                           :color-list ,test-colors-list)
                                           (((foreground . (default font-lock-keyword-face)))
                                           :persist t
                                           :next-color-func huecycle-get-random-color-from-list
                                           :color-list ,test-colors-list)
                                           (((background . (font-lock-keyword-face font-lock-builtin-face)))
                                           :persist t
                                           :next-color-func huecycle-get-random-color-from-list
                                           :color-list ,test-colors-list)
                                           (((background . (highlight font-lock-warning-face font-lock-string-face)))
                                           :persist t
                                           :next-color-func huecycle-get-random-color-from-list
                                           :color-list ,test-colors-list)
                                           (((distant-foreground . (font-lock-string-face default)))
                                           :persist t
                                           :next-color-func huecycle-get-random-color-from-list
                                           :color-list ,test-colors-list)))
     :huecycle-step-size 1
     :huecycle-iterations 5)))

(ert-deftest huecycle-uses-starting-color ()
  "Test huecycle uses a group's start color."
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal "#ff0000" (huecycle-get-face-color :foreground 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((foreground . default))
                                          :persist t
                                          :start-color "#ff0000"
                                          :interp-func huecycle-interpolate-step
                                          :next-color-func huecycle-get-random-color-from-list
                                          :color-list ,test-colors-list)))
   :huecycle-step-size 0.2
   :huecycle-iterations 1))

(ert-deftest huecycle-uses-speed ()
  "Test huecycle uses a group's speed"
  (huecycle-with-test-env
   (progn
     (huecycle)
     (should (equal "#000004" (huecycle-get-face-color :foreground 'highlight)))
     (should (equal "#000002" (huecycle-get-face-color :foreground 'default))))
   :huecycle--interpolate-data (mapcar (lambda (args) (apply #'huecycle--init-interp-datum args))
                                       `((((foreground . highlight))
                                          :persist t
                                          :speed 2.0
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))
                                         (((foreground . default))
                                          :persist t
                                          :next-color-func huecycle-get-next-list-color
                                          :color-list ("#000001" "#000002" "#000003" "#000004"))))
   :huecycle-step-size 0.5
   :huecycle-iterations 4))


(ert-deftest huecycle-test-active-buffers-size ()
  "Test that `huecycle--active-buffers' has buffers that it should have, and doesn't exceed `huecycle--max-active-buffers'."
  (huecycle-with-test-env
   (let* ((buffer-1 (get-buffer-create "huecycle-test-buffer-1"))
          (buffer-2 (get-buffer-create "huecycle-test-buffer-2"))
          (buffer-3 (get-buffer-create "huecycle-test-buffer-3"))
          (buffer-4 (get-buffer-create "huecycle-test-buffer-4"))
          (buffer-5 (get-buffer-create "huecycle-test-buffer-5"))
          (buffer-access-list (list
                               buffer-1
                               buffer-2
                               buffer-3
                               buffer-4
                               buffer-5)))
     (unwind-protect
         (progn
           (mapc (lambda (buf) (with-current-buffer buf (huecycle))) buffer-access-list)
           (should (= 3 (length huecycle--active-buffers))))
       (mapc #'kill-buffer buffer-access-list)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle--max-active-buffers 3)
  (huecycle-with-test-env
   (let* ((buffer-1 (get-buffer-create "huecycle-test-buffer-1"))
          (buffer-2 (get-buffer-create "huecycle-test-buffer-2"))
          (buffer-3 (get-buffer-create "huecycle-test-buffer-3"))
          (buffer-4 (get-buffer-create "huecycle-test-buffer-4"))
          (buffer-5 (get-buffer-create "huecycle-test-buffer-5"))
          (buffer-access-list (list
                               buffer-1
                               buffer-2
                               buffer-2
                               buffer-3
                               buffer-4
                               buffer-3
                               buffer-5
                               buffer-1
                               buffer-3
                               buffer-5)))
     (unwind-protect
         (progn
           (mapc (lambda (buf) (with-current-buffer buf (huecycle))) buffer-access-list)
           (should (= 5 (length huecycle--active-buffers))))
       (mapc #'kill-buffer buffer-access-list)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle--max-active-buffers 5)
(huecycle-with-test-env
   (let* ((buffer-1 (get-buffer-create "huecycle-test-buffer-1"))
          (buffer-2 (get-buffer-create "huecycle-test-buffer-2"))
          (buffer-3 (get-buffer-create "huecycle-test-buffer-3"))
          (buffer-4 (get-buffer-create "huecycle-test-buffer-4"))
          (buffer-5 (get-buffer-create "huecycle-test-buffer-5"))
          (buffer-access-list (list buffer-2)))
     (unwind-protect
         (progn
           (mapc (lambda (buf) (with-current-buffer buf (huecycle))) buffer-access-list)
           (should (= 1 (length huecycle--active-buffers))))
       (mapc #'kill-buffer buffer-access-list)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle--max-active-buffers 3))

(ert-deftest huecycle-test-active-buffers-ordering ()
  "Tests that `huecycle--active-buffers' is properly set to reflect access ordering."

  ;; Test buffers are ordered from oldest to newest
  (huecycle-with-test-env
   (let* ((buffer-1 (get-buffer-create "huecycle-test-buffer-1"))
          (buffer-2 (get-buffer-create "huecycle-test-buffer-2"))
          (buffer-3 (get-buffer-create "huecycle-test-buffer-3"))
          (buffer-4 (get-buffer-create "huecycle-test-buffer-4"))
          (buffer-5 (get-buffer-create "huecycle-test-buffer-5"))
          (buffer-access-list (list
                               buffer-1
                               buffer-2
                               buffer-3
                               buffer-4
                               buffer-5)))
     (unwind-protect
         (progn
           (mapc (lambda (buf) (with-current-buffer buf (huecycle))) buffer-access-list)
           (should (equal "huecycle-test-buffer-5" (buffer-name (nth 0 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-4" (buffer-name (nth 1 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-3" (buffer-name (nth 2 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-2" (buffer-name (nth 3 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-1" (buffer-name (nth 4 huecycle--active-buffers)))))
       (mapc #'kill-buffer buffer-access-list)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle--max-active-buffers 5)
  (huecycle-with-test-env
   (let* ((buffer-1 (get-buffer-create "huecycle-test-buffer-1"))
          (buffer-2 (get-buffer-create "huecycle-test-buffer-2"))
          (buffer-3 (get-buffer-create "huecycle-test-buffer-3"))
          (buffer-4 (get-buffer-create "huecycle-test-buffer-4"))
          (buffer-5 (get-buffer-create "huecycle-test-buffer-5"))
          (buffer-access-list (list
                               buffer-1
                               buffer-2
                               buffer-3
                               buffer-4
                               buffer-5
                               buffer-1
                               buffer-3)))
     (unwind-protect
         (progn
           (mapc (lambda (buf) (with-current-buffer buf (huecycle))) buffer-access-list)
           (should (equal "huecycle-test-buffer-3" (buffer-name (nth 0 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-1" (buffer-name (nth 1 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-5" (buffer-name (nth 2 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-4" (buffer-name (nth 3 huecycle--active-buffers))))
           (should (equal "huecycle-test-buffer-2" (buffer-name (nth 4 huecycle--active-buffers)))))
       (mapc #'kill-buffer buffer-access-list)))
   :huecycle--interpolate-data (mapcar #'huecycle--init-interp-datum '(((foreground . default))))
   :huecycle--max-active-buffers 5))
