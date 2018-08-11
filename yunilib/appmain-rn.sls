(library (appmain-rn)
         (export main)
         (import (yuni scheme) (testlib a))

(define js-load-async (yuni/js-import "js-load-async"))
(define (delay-load libname)
  (vector-ref (yuni/js-invoke/async1 js-load-async libname) 0))

(define %thiswrap (yuni/js-import "thiswrap"))
(define-syntax wrap-this   ;; Yuni generic-runtime requires 
  (syntax-rules ()         ;; EVERY syntax-rules as top-level form
    ((_ this form ...)
     (js-call %thiswrap (js-closure (lambda (this) form ...))))))

;; Globals
(define e (yuni/js-import "e"))
;; Delayed initialized components
(define Mainapp #f)

(define (init-classes!) 
  (PCK 'INIT)
  (define count 0)
  (define theCounter (js-obj "count" count))
  (define (count++)
    (set! count (+ count 1))
    (js-set! theCounter "count" count)
    theCounter)
  (let ((createReactClass (yuni/js-import "createReactClass"))
        (libs (yuni/js-import "rn-libs")))
    (define (l sym)
      (let ((r (js-ref libs (if (symbol? sym) (symbol->string sym) sym))))
       (PCK 'LIB sym '=> r)
       r))
    (let ((Text (l 'Text))
          (View (l 'View)))
      (define (main-app)
        (js-call 
          createReactClass
          (js-obj "render"
                  (wrap-this
                    _
                    (js-call e View
                             #f
                             (js-call e Text #f "Line1")
                             (js-call e Text #f "Line2")
                             (js-call e Text #f "Line3"))))))
      (set! Mainapp (main-app)))))

(define (main)
  (init-classes!)
  Mainapp)

(PCK 'LOAD-RN)
)
