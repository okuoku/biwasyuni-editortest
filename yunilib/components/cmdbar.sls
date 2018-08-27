(library (components cmdbar)
         (export cmdbar)
         (import (yuni scheme)
                 (react-mui)
                 (reactutil)
                 (jsutil))
         
;;

(define cmdbar
  (let ((P (js-obj "variant" "contained" "color" "primary" "disabled" #t))
        (S (js-obj "variant" "contained" "color" "secondary" "disabled" #t)))
    (make-react-class
      "render" (wrap-this this
                          (ReactDiv
                            #f
                            (Button P "Link")
                            (Button S "Uncheck-all"))))))
         
)
