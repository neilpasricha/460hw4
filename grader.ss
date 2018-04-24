;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                            ;;;;
;;;;   DO NOT MODIFY ANYTHING IN THIS FILE      ;;;;
;;;;   DO NOT SUBMIT THIS FILE                  ;;;;
;;;;                                            ;;;;
;;;;   To run all the test cases in tests.ss    ;;;;
;;;;      > (run-tests)                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang mzscheme

(require "hw4.ss")
(require "tests.ss")

(require (lib "pretty.ss"))


(define scheme-val->expval
  (lambda (sval)
    (cond
      ((number? sval) (num-val sval))
      ((boolean? sval) (bool-val sval))
      (else (error
              'equal??
              "Can't convert the correct answer to expval: ~s"
              sval)))))


(define equal??
  (lambda (result correct-ans)
    (let ((answer (scheme-val->expval correct-ans)))
      (equal? result answer))))




(define stop-after-first-error (make-parameter #f))
(define run-quietly (make-parameter #t))


(define apply-safely
  (lambda (proc args)
    (with-handlers (((lambda (exn) #t)      ; catch any error
                     (lambda (exn)          ; evaluate to a failed test result
                       (cons #f 
                         (if (exn? exn)
                             (exn-message exn)
                             exn)))))  
      (let ((actual (apply proc args)))
        (cons #t actual)))))



(define run-a-test
  (lambda (proc args correct-ans equal-ans?)
    (let* ((result (apply-safely proc args))   ; result is either the answer or args to eopl:error
           (error-thrown? (not (car result)))
           (ans (cdr result)))
      (cons
        (if (eqv? correct-ans 'error)
            error-thrown?
            (equal-ans? ans correct-ans))
        ans))))


(define run-tests
  (lambda ()
    (let ((tests-passed '())
          (tests-failed '()))
      (for-each
       (lambda (a-test)
         (let ((name (car a-test))
               (pgm (cadr a-test))
               (correct-ans (caddr a-test)))
           (printf "test: ~a~%" name)
           (let* ((result (run-a-test run (list pgm) correct-ans equal??))
                  (correct? (car result))
                  (actual-ans (cdr result)))
             (if (or (not correct?)
                     (not (run-quietly)))
                 (begin
                   (printf "~a~%" pgm)
                   (printf "correct outcome: ~a~%" (scheme-val->expval correct-ans))
                   (printf "actual outcome: ")
                   (pretty-display actual-ans)))
             (if correct?
                 (begin
                   (printf "correct~%~%")
                   (set! tests-passed (cons name tests-passed)))
                 (begin
                   (printf "incorrect~%~%")
                   (if (stop-after-first-error)
                       (error name "incorrect outcome detected"))
                   (set! tests-failed
                         (cons name tests-failed)))))))
       test-list)
      (begin
        (printf "out of ~a test cases~%" (length test-list))
        (printf " - passed: ~a~%" (length tests-passed))
        (printf " - failed: ~a~%~%" (length tests-failed))
        (if (null? tests-failed)
          (printf "Nice work~%")
          (printf "Failed tests: ~a~%" (reverse tests-failed)))))))

             
                 
