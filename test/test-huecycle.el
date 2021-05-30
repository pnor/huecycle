;; test-huecycle.el --- Tests for huecycle -*- no-byte-compile: t; -*-

(require 'huecycle)
(require 'ert)

;; what to test

;; TODO Huecycle general tests
;; - TODO general test with various configs
;; - doesn't lose cookies

;; setup all globals
;; specify in 1 term how long to huecycle for (step-size * huecycle-counter) = this value in secs
(defmacro hueycle-test-env ()
  )

(huecycle-test-env
 :huecycle-iterations 5
 (huecycle)
 )

(cl-defmacro huecycle-test-env
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
           ,body)
         )
       )))


;; general form
;; - setup let bindings for globals
;; - run huecycle for some time (in temp buffer)
;; - assert cookies are what you want
;; Generaly huecycle should
;; - update progress correctly
;; - set faces correctly
;; - cleanup properly
;; TODO huecycle-set-faces sets correctly
;; - maps input call to list of interp datum correctly

;; TODO split huecycle into the setup, bulk, and tear down and test each
(ert-deftest huecycle-test-huecycle-runs ()
  "Test that huecycle runs without errors"
  ;; Bind all global variables
  (let  (
         (huecycle-counter 0) ;; Counter for input pending, must be > 1 to terminate
         (huecycle-cycle-duration 0) ;; default
         (huecycle-step-size 0.033333)
         (huecycle--interpolate-data '())
         (huecycle--buffer-data '())
         (huecycle--interpolate-data '())
         (huecycle--buffer-data '())
         (huecycle--active-buffers '())
         (huecycle--max-active-buffers 10)
         (huecycle--idle-timer nil)
         (huecycle--current-time 0)
         (huecycle--default-start-color "#888888")
         )
    ;; Bind all functions
  (cl-flet ((huecycle--input-pending ()
               (if (>= huecycle-counter 4) ;; replace 4 with how many cycles
                   t
               (setq huecycle-counter (1+ huecycle-counter))
               nil)
             ))
    ;; IN a temp buffer, do the work
(with-temp-buffer
  (huecycle) ;; the work
  )
  )
  ))

(ert-deftest bad-test ()
  ""
  (should (= 1 1)))

;; TODO reset all faces does as advertised

;; TODO buffer management
;; - call huecycle across buffers and see assert active buffers is correct order
