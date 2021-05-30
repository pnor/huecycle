;; test-huecycle.el --- Tests for huecycle -*- no-byte-compile: t; -*-

(require 'huecycle)
(require 'ert)

(cl-defmacro huecycle-with-test-env
    (body &key
          (huecycle-iterations 1)
          (huecycle-interpolate-data '())
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
BODY is a single lisp form, to run multiple statements use `progn'."
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
           (huecycle-interpolate-data ,huecycle-interpolate-data)
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

(ert-deftest huecycle-test-huecycle-runs ()
  "Test that huecycle runs without errors"
  (huecycle-with-test-env
   (huecycle-with-test-env
    (condition-case err
        (huecycle)
      (t nil)
      (:success t)))))
