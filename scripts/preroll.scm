;; 
;; Preroll: Generate library list for debug-run
;; 

(define *fs* (yuni/js-import "fs"))

(define *dirs*
  '("yuni/lib-compat"
    "yuni/lib"
    "yunilib"))

(define (file->sexp-list fn)
  ;; FIXME: Workaround...
  (define (wrap-paren str)
    (string-append "(" str ")"))
  (let ((p (open-input-string (wrap-paren 
                                (js-invoke *fs* "readFileSync" fn "utf8")))))
    (read p)))

(define (string->file fn str)
  (js-invoke *fs* "writeFileSync" fn str))

(define (is-sls? fn)
  (let ((len (string-length fn)))
   (and (< 3 len)
        (string=? ".sls" (substring fn (- len 4) len)))))

(define (is-directory? fn)
  (let ((stat (js-invoke *fs* "statSync" fn)))
   (js-invoke stat "isDirectory")))

(define (prefix fn l)
  (map (lambda (e) (string-append fn "/" e)) l))

(define (directory->file-list fn)
  (let ((l (prefix fn (vector->list (js-invoke *fs* "readdirSync" fn)))))
   (let loop ((q l)
              (files '()))
     (if (pair? q)
       (let ((next (cdr q))
             (f (car q)))
         (if (is-directory? f)
           (let ((x (directory->file-list f)))
            (loop next (append x files)))
           (if (is-sls? f)
             (loop next (cons f files))
             (loop next files))))
       files))))

(unless (file-exists? "dist")
  (js-invoke *fs* "mkdirSync" "dist"))

(let ((p (open-output-string)))
 (define cfg (file->sexp-list "yuni/config/generic-runtime.scm"))
 (define (emit-load l)
   (for-each (lambda (e)
               (write (list 'load (string-append "yuni/" e)) p)
               (newline p)) 
             l))
 ;; Emit prelude
 (write '(define (command-line) (list "" "" "" "" "")) p)
 (newline p)

 ;; Emit runtime library load
 (let ((generic (assoc 'generic cfg))
       (biwascheme (assoc 'biwascheme cfg)))
   (emit-load (cadr biwascheme))
   (emit-load (cadr generic))
   (emit-load (caddr generic))
   (emit-load (caddr biwascheme)))

 ;; Emit library file list
 (for-each (lambda (e)
             (write (list 'yuni/add-library-override-prefix! e) p)
             (newline p))
           *dirs*)
 (let ((filelist (apply append (map directory->file-list *dirs*))))
  (for-each (lambda (e) 
              (write (list 'yuni/add-library-override-path! e) p) 
              (newline p))
            filelist))
 (newline p)
 (write (list 'load "app.sps") p)
 (newline p)
 (string->file "dist/boot.scm" (get-output-string p)))

