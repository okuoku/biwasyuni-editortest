(library (appmain)
         (export main)
         (import (yuni scheme))

         
(define (main) 
  (PCK 'INIT)
  (let ((m (yuni/js-import "m"))
        (root (yuni/js-import "document-root")))
    (js-invoke m 'render root "Working...")
    (let* ((x (yuni/js-import "js-load-async"))
           (y (yuni/js-invoke/async1 x "prosemirror")))
      (display y)
      (newline))))

(PCK 'LOAD)
         
)
