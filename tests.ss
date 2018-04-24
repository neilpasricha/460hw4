;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      CMPSC 460-001                                         ;;;
;;;      Homework 4                                            ;;;
;;;      Neil Pasricha                                         ;;;
;;;      nvp5119                                               ;;;
;;;                                                            ;;;
;;;      Note:                                                 ;;;
;;;        Have a great day.                                   ;;;
;;;                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Use the following test cases for initial testing.
;;;; Once you are done, remove the provided test cases
;;;;   and add at least 15 test cases of your own for
;;;;   each of the following categories.


(module tests mzscheme
  
  (provide test-list)

  (define test-list
    '(
      ;; const-exp
      (const-exp-1 "100" 100)
      (const-exp-2 "49" 49)
      (const-exp-3 "0" 0)
      (const-exp-4 "-57" -57)
      (const-exp-5 "-111" -111)

      ;; var-exp
      (var-exp-1 "x" 10)
      (var-exp-2 "v" 5)
      (var-exp-3 "i" 1)
      (var-exp-4 "y" error)
      (var-exp-5 "foo" error)

      ;; zero?-exp
      (zero?-exp-1 "zero? (0)" #t)
      (zero?-exp-2 "zero? (1)" #f)
      (zero?-exp-3 "zero? (-(x,x))" #t)
      (zero?-exp-4 "zero? (-(x,v))" #f)
      (zero?-exp-5 "zero? (y)" error)

      ;; diff-exp
      (diff-exp-1 "-(x,x)" 0)
      (diff-exp-2 "-(x,i)" 9)
      (diff-exp-3 "-(i,x)" -9)
      (diff-exp-4 "-(zero(9),x" error)
      (diff-exp-5 "-(10,-(5,-(7,5)))" 7)

      ;; if-exp
      (if-exp-1 "if zero?(0) then 3 else 4" 3)
      (if-exp-2 "if zero?(1) then 3 else 4" 4)
      (if-exp-3 "if zero? (-(x, 11)) then -(v, 2) else -(v, 4)" 1)
      (if-exp-4 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-exp-5 "if zero?(-(11,12)) then foo else 4" 4)

      ;; let-exp
      (let-exp-1 "let x = 3 in x" 3)
      (let-exp-2 "let x = 3 in -(x,1)" 2)
      (let-exp-3 "let x = -(4,1) in -(x,1)" 2)
      (let-exp-4
        "let x = 33
         in let y = 22
            in if zero? (-(x, 11))
               then -(y, 2)
               else -(y, 4)"
        18)
      (let-exp-5
        "let x = 7
         in let y = 2
            in let y = let x = -(x, 1)
                       in -(x, y)
               in -(-(x, 8), y)"
        -5)

      ;; call-exp
      (call-exp-1 "(proc(x) -(x,1)  30)" 29)
      (call-exp-2 
        "let f = proc(x) -(x, 11)
         in (f (f 7))"
        -15)
      (call-exp-3 
        "(proc (f) (f (f 77))       
          proc (x) -(x, 11))"
        55)
      (call-exp-4 "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (call-exp-5 
        "let fix =  proc (f)
                      let d = proc (x) proc (z) ((f (x x)) z)
                      in proc (n) ((f (d d)) n)
         in let t4m = proc (f)
                        proc(x)
                          if zero?(x) then 0 else -((f -(x,1)),-4)
            in let times4 = (fix t4m)
               in (times4 3)"
        12)
      (call-exp-6 "let a = 5
                  in let p = proc (x) -(x, a)
                     in let a = 10
                        in -(a, (p 2))"
        13)
      (call-exp-7 "let a=3
                  in let p = proc (z) a
                     in let f = proc (x) (p 0)
                        in let a = 5
                           in (f 2)"
        3)
      (call-exp-8 "let a=3
                  in let p = proc (z) a
                     in let f = proc (a) (p 0)
                        in let a = 5
                           in (f 2)"
        3)

      ;; letrec-exp
      (letrec-exp-1 
        "letrec double(x) = if zero? (x)
                            then 0
                            else -((double -(x,1)), -2)
         in (double 2)"
        4)
      (letrec-exp-2 
        "letrec double(x) = if zero? (x)
                            then 0
                            else -((double -(x,1)), -2)
         in (double 6)"
        12)
      (letrec-exp-3 
        "let m = -5 
         in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m)
            in (f 4)"
        20)
      (letrec-exp-4 
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)
      (letrec-exp-5 
        "letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
         in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
            in (odd 13)"
        1)


      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;    TEST CASES FOR EXTENSIONS                           ;;;;
      ;;;;                                                        ;;;;
      ;;;;    - Uncomment and Use these test cases for initial    ;;;;
      ;;;;        testing of your extended interpreter            ;;;;
      ;;;;    - Once you are done, remove these test cases        ;;;;
      ;;;;        and add at least 15 test cases for each         ;;;;
      ;;;;        extension                                       ;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      

      ;; multiple let
      (new-let-1
        "let
         in  -(12,2)"
         10)
      (new-let-2
        "let x = 10
         in let x = 20
            in -(x, 10)"
        10)
      (new-let-3
        "let x = 0
                y = 2
                z = 3
         in if zero? (x)
            then y
            else z"
        2)
      (new-let-4
        "let x = 30
         in let x = -(x,1)
                y = -(x,2)
            in -(x, y)"
        1)
      (let-exp-5
        "let x = 33
             y = 22
         in if zero? (-(x, 11))
            then -(y, 2)
            else -(y, 4)"
        18)
      (let-exp-6
        "let x = 7
             y = 2
         in let y = let x = -(x, 1)
                    in -(x, y)
            in -(-(x, 8), y)"
        -5)

      (new-let-7
        "let
         in  -(15,3)"
         12)

      (new-let-8
        "let
         in  -(5,2)"
         3)

      (new-let-9
        "let
         in  -(20,2)"
         18)

      (new-let-10
        "let
         in  -(600,2)"
         598)

      (new-let-11
        "let
         in  -(2,1)"
         1)

      (new-let-12
        "let
         in  -(2,2)"
         0)

      (new-let-13
        "let
         in  -(22,21)"
         1)

      (new-let-14
        "let
         in  -(18,1)"
         17)

      ;; call with multiple args
      (new-call-exp-1
        "(proc () 10)"
        10)
      (new-call-exp-2
        "(proc (x) -(x,1)
          10)"
        9)
      (new-call-exp-3
        "(proc (x, y) -(x, y)
          10 8)"
        2)
      (new-call-exp-4
        "(proc(x, y) -(x, y) 10 20)"
        -10)
      (new-call-exp-5
        "(proc (x, y) -(x,y) 5 6)"
        -1)
      (new-call-exp-6
        "(proc (x,y,z) -(x,-(y,z))
          10 5 2)"
        7)
      (new-call-exp-7
        "let p=proc(x,y) -(x,y)
         in (p 10 2)"
        8)
      (new-call-exp-8
        "let x = 10
         in let f = proc () -(x, 5)
            in (f)"
        5)
      (new-call-exp-9
        "let f=proc(x,y,z) -(-(x,y),z)
         in (f 10 5 2)"
        3)
      (new-call-exp-10
        "let f = proc (x) proc (y) -(x, -(0, y)) in ((f 10) 20)"
        30)

      (new-call-exp-11
        "(proc () 1)"
        1)

      (new-call-exp-12
        "(proc (x) 10)"
        10)

      (new-call-exp-13
        "(proc (y) 10)"
        10)

      (new-call-exp-14
        "(proc (z) 1)"
        1)

      (new-call-exp-15
        "(proc (x) 5)"
        5)

      (new-call-exp-16
        "(proc (y) 120)"
        120)

      (new-call-exp-17
        "(proc (z) 450)"
        450)

      (new-call-exp-18
        "(proc () 999)"
        999)
      
      
      
      )
    )
  )



