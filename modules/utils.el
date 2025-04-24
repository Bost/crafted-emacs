;;; utils.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Rostislav Svoboda

;; Author: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; Keywords:

;; See lisp/subr.el

(defmacro my-with-eval-after-load (file &rest body)
  "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See `eval-after-load'
for more details about the different forms of FILE and their semantics."
  (declare (indent 1) (debug (form def-body)))
  `(progn
     ;; (message "(boundp 'context): %s " (boundp 'context))
     (when (boundp 'context) (setq-local old-context context))
     (setq-local context (format "my-with-eval-after-load %s:" ,file))
     (message "%s registering ..." context)
     ;; (eval-after-load ,file (lambda () ,@body))
     (eval-after-load ,file
       (lambda ()
         ;; --- Tracking starts here (when the lambda executes) ---
         (message "--> Executing eval-after-load body for %s..." ,file)
         ;; Use condition-case to ensure 'finished' message even on error
         (condition-case err
             (progn ; Evaluate the original body code
               ,@body
               (message "<-- Finished eval-after-load body for %s." ,file))
           (error
            (message "<-- Error executing eval-after-load body for %s: %S" ,file err)
            ;; Resignal the error so it's not silently swallowed
            (signal (car err) (cdr err))))
         ))
     ;; (message "%s registration done" context)
     (when (boundp 'old-context) (setq-local context old-context))))

(defmacro my-with-eval-after-load (file &rest body)
  "Like `with-eval-after-load', but log invocation and execution timing."
  (declare (indent 1))  ; allow body to be indented like with-eval-after-load
  ;; Capture the invocation time at macro expansion
  (let ((invocation-time (current-time)))
    ;; Log the macro invocation (scheduling of after-load code)
    (message "my-with-eval-after-load: Scheduling code for %S at %s"
             file (current-time-string invocation-time))
    ;; Return the expanded code that defers execution and logs when running
    `(eval-after-load ,file
       ;; Use a lambda to execute the body so we can log at run-time
       (lambda ()
         (let ((delay (float-time (time-subtract (current-time) ',invocation-time))))
           (message "my-with-eval-after-load: Running deferred code for %S at %s (%.3f sec delay)"
                    ,file (current-time-string) delay))
         ;; Measure execution time of the body as well (optional):
         (let ((start (current-time)))
           ,@body
           (let ((elapsed (float-time (time-subtract (current-time) start))))
             (message "my-with-eval-after-load: Finished body for %S (took %.3f sec)"
                      ,file elapsed)))))))

(provide 'utils)
