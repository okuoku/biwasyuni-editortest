(library (appmain)
         (export main)
         (import (yuni scheme))

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
(define d (yuni/js-import "d"))
;; Delayed initialized components
(define Mainapp #f)

(define (pp obj)
  (js-call (yuni/js-import "pp") obj))

(define genthisref
  (let ((g (yuni/js-import "genthisref")))
   (lambda () (js-call g))))
         
(define (init-classes!) 
  (PCK 'INIT)
  (define count 0)
  (define theCounter (js-obj "count" count))
  (define (count++)
    (set! count (+ count 1))
    (js-set! theCounter "count" count)
    theCounter)
  (let ((createReactClass (yuni/js-import "createReactClass"))
        (mui (delay-load "materialui")))
    (define (m sym)
      (let ((r (js-ref mui (if (symbol? sym) (symbol->string sym) sym))))
       (PCK 'MUI sym '=> r)
       r))
    (let ((CssBaseline (m 'CssBaseline))
          (AppBar      (m 'AppBar))
          (Toolbar     (m 'Toolbar))
          (Typography  (m 'Typography))
          (Button      (m 'Button))
          (ReactFragment (yuni/js-import "ReactFragment")))
      (define (counter-object)
        (js-call 
          createReactClass
          (let* ((this0 #f)
                 (handleClick (js-closure
                                (lambda (e)
                                  (count++)
                                  (PCK 'COUNT count this0)
                                  (when this0
                                    (js-invoke this0 "setState" theCounter))))))
            (js-obj
              "render" (wrap-this this
                                  (unless this0
                                    (set! this0 this))
                                  (js-call e Button
                                           (js-obj "color" "primary"
                                                   "onClick" handleClick)
                                           (number->string
                                             (js-ref (js-ref this "state")
                                                     "count"))))

              "handleClick" handleClick
              "getInitialState" (js-closure (lambda () theCounter))))))
      (define (main-app)
        (js-call 
          createReactClass
          (js-obj "render"
                  (wrap-this
                    _
                    (js-call e ReactFragment
                             #f
                             (js-call e CssBaseline)
                             (js-call e AppBar 
                                      (js-obj "position" "static")
                                      (js-call e Toolbar
                                               (js-obj "variant" "dense")
                                               (js-call e Typography
                                                        #f
                                                        "Title")))
                             (js-call e (counter-object) theCounter))))))

      (set! Mainapp (main-app)))))

(define (main)
  (init-classes!)
  (js-invoke d "render" (js-call e Mainapp) (yuni/js-import "document-root")))

(PCK 'LOAD)
)
