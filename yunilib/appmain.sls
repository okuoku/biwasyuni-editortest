(library (appmain)
         (export main)
         (import (yuni scheme))

(define (counter-object m)
  (define counter 0)
  (define (onclick e)
    (PCK 'ONCLICK counter e)
    (set! counter (+ counter 1)))
  (let ((handler (js-closure onclick)))
   (js-obj "view"
           (js-closure (lambda ()
                         (js-call m "div.alert.alert-primary[role=alert]" 
                                  (js-obj "onclick" handler)
                                  (number->string counter)))))))

(define (static-component obj)
  (js-obj "view"
          (js-closure (lambda () obj))))

(define (scene m counter) ;; => mVNODE
  (js-obj 
    "view" 
    (js-closure 
      (lambda ()
        (js-call m "div.container-fluid"
                 (js-call m "nav.navbar.navbar-expand-lg.navbar-light.bg-light"
                          (js-call m "a.navbar-brand" "Navbar")
                          (js-call m "a.navbar-brand" "Navbar")
                          (js-call m "a.navbar-brand" "Navbar")
                          (js-call m "a.navbar-brand" "Navbar"))
                 (js-call m counter))))))
         
(define (main) 
  (PCK 'INIT)
  (let ((m (yuni/js-import "m"))
        (root (yuni/js-import "document-root")))
    (js-invoke m 'render root "Working...")
    (let* ((x (yuni/js-import "js-load-async"))
           (y (yuni/js-invoke/async1 x "prosemirror"))
           (theCounter (counter-object m)))
      (js-invoke m 'mount root (scene m theCounter))
      (PCK 'RENDERD))))

(PCK 'LOAD)
         
)
