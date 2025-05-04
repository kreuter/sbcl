;;;; Tests for async signal safety.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(use-package :test-util)

(sb-ext:finalize (list 1) (lambda ()))
(with-test (:name (:async-unwind :specials)
                  :skipped-on (:and :sb-safepoint :linux)) ; hangs
  (let ((*x0* nil) (*x1* nil) (*x2* nil) (*x3* nil) (*x4* nil))
    (declare (special *x0* *x1* *x2* *x3* *x4*))
    (loop repeat 10 do
          (loop repeat 10 do
                (catch 'again
                  (sb-ext:schedule-timer (sb-ext:make-timer
                                          (lambda ()
                                            (throw 'again nil)))
                                         (random 0.1))
                  (loop
                   (let ((*x0* (cons nil nil)) (*x1* (cons nil nil))
                         (*x2* (cons nil nil)) (*x3* (cons nil nil))
                         (*x4* (cons nil nil)))
                     (declare (special *x0* *x1* *x2* *x3* *x4*)))))
                (when (not (and (null *x0*) (null *x1*) (null *x2*) (null *x3*)
                                (null *x4*)))
                  (format t "~S ~S ~S ~S ~S~%" *x0* *x1* *x2* *x3* *x4*)
                  (assert nil)))
          (princ '*)
          (force-output))
    (terpri)))

(require :sb-posix)

(with-test (:name (:signal :errno)
                  ;; This test asserts that nanosleep behaves correctly
                  ;; for invalid values and sets EINVAL.  Well, we have
                  ;; nanosleep on Windows, but it depends on the caller
                  ;; (namely SLEEP) to produce known-good arguments, and
                  ;; even if we wanted to check argument validity,
                  ;; integration with `errno' is not to be expected.
                  ;; And this hangs on darwin + safepoint.
                  :skipped-on (or :win32 (:and :darwin :sb-safepoint)))
  (let* (saved-errno
         (returning nil)
         (timer (make-timer (lambda ()
                              (sb-unix:unix-open "~!@#$%^&*[]()/\\" 0 0)
                              (assert (= sb-unix:enoent
                                         (sb-unix::get-errno)))
                              (setq returning t)))))
    (schedule-timer timer 0.2)
    ;; Fail and set errno.
    (sb-unix:nanosleep -1 -1)
    (setq saved-errno (sb-unix::get-errno))
    (assert (= saved-errno sb-posix:einval))
    ;; Wait, but not with sleep because that will be interrupted and
    ;; we get EINTR.
    (loop until returning)
    (assert (= saved-errno (sb-unix::get-errno)))))

;; It is desirable to support C-c on Windows, but SIGINT
;; is not the mechanism to use on this platform.
;; This test used to call kill_safely() in the C runtime if using safepoints,
;; and perhaps at some point kill_safely() interacted with the safepoint state
;; for POSIX (i.e. not win32), but it doesn't, at least not now.
;; The special case in kill_safely() for the current thread is pthread_kill()
;; and not a thing more, unless on win32, which skips this test.
;; Note also that RAISE sends a thread-directed signal as per the man page
;; "In a multithreaded program it is equivalent to pthread_kill(pthread_self(), sig);"
;; but thread-directed SIGINT is not the right thing, as it does not accurately
;; model the effect of pressing control-C; hence we should use UNIX-KILL here,
;; which sends a process-directed signal, letting the OS pick a thread.
;; Whether it picks the finalizer thread or main thread, things should work,
;; because we forward to the signal to our foreground thread.
#+unix
(with-test (:name :handle-interactive-interrupt)
  (assert (eq :condition
              (handler-case
                  (progn
                    (sb-unix:unix-kill (sb-unix:unix-getpid) sb-unix:sigint)
                    #+sb-safepoint
                    ;; In this case, the signals handler gets invoked
                    ;; indirectly through an INTERRUPT-THREAD.  Give it
                    ;; enough time to hit.
                    (sleep 1))
                (sb-sys:interactive-interrupt ()
                  :condition)))))

(with-test (:name :bug-640516
                  :skipped-on :gc-stress)
  ;; On Darwin interrupting a SLEEP so that it took longer than
  ;; the requested amount caused it to hang.
  (assert
   (handler-case
       (sb-ext:with-timeout 10
         (let (to)
           (handler-bind ((sb-ext:timeout (lambda (c)
                                            (unless to
                                              (setf to t)
                                              (sleep 2)
                                              (continue c)))))
             (sb-ext:with-timeout 0.1 (sleep 1) t))))
     (sb-ext:timeout ()
       nil))))

#+unix
(with-test (:name :ignore-sigpipe)
  (multiple-value-bind (read-side write-side) (sb-unix:unix-pipe)
    (sb-unix:unix-close read-side)
    (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)
    (let ((buffer "x"))
      (sb-sys:with-pinned-objects (buffer)
        (multiple-value-bind (nbytes errno)
            (sb-unix:unix-write write-side buffer 0 1)
          (assert (and (null nbytes)
                       (= errno sb-unix:epipe))))))
    (sb-unix:unix-close write-side)))


;; WITHOUT-INTERRUPTS saves signal data and runs a handler in a later
;; unwind. If a fork() happens between save and unwind, the child
;; should not run the handler. This is for agreement with POSIX fork()
;; "The set of signals pending for the child process shall be
;; initialized to the empty set."
;; https://pubs.opengroup.org/onlinepubs/9799919799/functions/fork.html

#+unix ;unreadable on Windows b/c some of sb-posix doesn't exist
(with-test (:name :fork-during-without-interrupts)
  (flet ((fork* ()
           ;; Ensure the parent gets a SIGINT before the syscall.
           ;; If necessary, put a (sleep <x>) after to give the OS time
           ;; to deliver the self-interrupt.
           (sb-posix:kill (sb-posix:getpid) sb-posix:sigint)
           ;; Note: this does not do everything sb-posix:fork does.
           ;; Not trying to be a complete binding to fork(), just a
           ;; tool for exercising WITHOUT-INTERRUPTS.
           (let ((pid
                  (sb-alien:alien-funcall
                   (sb-alien:extern-alien "fork" (function sb-alien:int)))))
             (when (zerop pid)
               (sb-alien:alien-funcall
                (sb-alien:extern-alien
                 "sb_posix_after_fork" (function sb-alien:void))))
             pid))
         (waitpid* (pid)
           (multiple-value-bind (ignore status) (sb-posix:waitpid pid 0)
             (declare (ignore ignore))
             (let ((exited (sb-posix:wifexited status)))
               (values (if exited :exited :signaled)
                       (if exited
                           (sb-posix:wexitstatus status)
                           (sb-posix:wtermsig status)))))))
    (let (child handled)
      (block nil
        (handler-bind
            ((sb-sys:interactive-interrupt
              (lambda (ignore)
                (declare (ignore ignore))
                (setq handled t)
                (return))))
          (when (zerop
                 ;; This WITHOUT-INTERRUPTS both ensures CHILD gets
                 ;; assigned in the parent, and causes the parent to
                 ;; save the interrupt data for later processing.
                 (sb-sys:without-interrupts (setq child (fork*)))
                 ;; In both the parent and the child, the SIGINT
                 ;; will be processed "here", so to speak.
                 )
            ;; XXX: without :RECKLESSLY-P the child never exits?
            ;; (:RECKLESSLY-P T doesn't affect this test, though.)
            (sb-ext:quit :unix-status 0 :recklessly-p t))))
      (etypecase  child
        ((eql 0)
         ;; Same here for :RECKLESSLY-P,
         (sb-ext:quit :unix-status 1 :recklessly-p t))
        ((integer 1)
         (multiple-value-bind (exited number)
             (waitpid* child)
           (assert handled)
           (assert (eql :exited exited))
           (assert (zerop number))))))))
