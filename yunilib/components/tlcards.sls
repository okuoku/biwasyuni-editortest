(library (components tlcards)
         (export
           tlcard/large)
         (import (yuni scheme)
                 (react-mui)
                 (reactutil)
                 (jsutil))

         
;;

(define %tlcard/logdata
  (make-react-class
    "render" (wrap-this this
                        (ReactPre #f (js-ref (js-ref this "props")
                                               "value")))))

(define tlcard/large
  (make-react-element
    ((withStyles 
       (js-obj "card" (js-obj "maxWidth" "350px")))
     (make-react-class/raw
       "render" (wrap-this this
                           (let* ((props (js-ref this "props"))
                                  (classes (js-ref props "classes")))
                             (Card
                               (js-obj "className" (js-ref classes "card"))
                               (CardHeader
                                 (js-obj "avatar" (Avatar #f "Q")
                                         "action" (Checkbox)
                                         "title" (js-ref props "ident")))
                               (CardContent
                                 #f
                                 (%tlcard/logdata
                                   (js-obj "value"
                                           (js-ref props "message")))))))))))

)
