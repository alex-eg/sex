; Why `sex-modules`? Probably `modules` unit is reserved by chicken,
; things start to break.
(declare (unit sex-modules)
         (uses utils))

(import brev-separate
        (chicken file)
        (chicken load)
        (chicken pathname)
        (chicken string)
        fmt
        srfi-1)

(define +persistent-module-paths+ (list))

(define (import-modules module-list)
  ;; Module list is a list of symbols
  ;; How Sex handles modules:
  ;; For each module in a list, construct path, find module by path in
  ;; module path directories, extract public definitions from the
  ;; module, paste them in current one in emulation of C include
  ;; directives.
  (fold-right append (list)
              (map (fn (import-module (symbol->string x)))
                   module-list)))

(define (import-module name)
  (let ((module-path (locate-module name)))
    (assert module-path (fmt #f "Failed to find module " name " in "
                             (get-module-paths)))
    (read-public-interface module-path)))

(define (get-module-paths)
  (cons (current-directory)
        +persistent-module-paths+))

(define (locate-module name)
  ;; Module locations: relative to file being compiled, or in what was
  ;; in SEX_MODULE_PATH env var at the start of the process (see
  ;; load-persistent-module-paths function)

  (let ((search-paths (get-module-paths)))
    (let loop ((paths search-paths))
      (if (null? paths)
          #f
          (or (module-exists? name (car paths))
              (loop (cdr paths)))))))

(define (module-exists? name module-dir)
  ;; returns absolute path to module, if it exists
  (and (directory-exists? module-dir)
       (let ((module-path (make-absolute-pathname module-dir name "sex")))
         (and (file-exists? module-path)
              (file-readable? module-path)
              module-path))))

(define (read-public-interface module-path)
  ;; pub fns are reduced to prototypes, other pub forms are just pasted
  (let ((raw-forms (read-from-file module-path)))
    (fold
     process-public-interface-form
     (list)
     raw-forms)))

(define (process-public-interface-form form acc)
  (case (car form)
    ((pub)
     (case (cadr form)
       ((fn) ; replace with prototype
        ;; fn type name (arg-list) (body)
        ;; 1    2   3       4 - we need first 4
        (cons (take (cdr form) 4) acc))
       ((define import include struct template typedef union var)
        (cons (cdr form) acc))
       (else (error "Pub what? " (cadr form)))))
    (else acc)))

(define (load-persistent-module-paths)
  (let ((sex-module-path-env-var
         (get-env-var "SEX_MODULE_PATH")))
    (when sex-module-path-env-var
      (set! +persistent-module-paths+
        (map (lambda (p)
               (make-absolute-pathname p #f #f))
             (string-split sex-module-path-env-var ":"))))))
