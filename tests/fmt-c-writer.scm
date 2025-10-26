;;; Types
(test-group "fmt-writer"

  (test
   '(const int)
   (walk-type '(const int)))

  (test
   '(%array (const char) 512)
   (walk-type '(¤ const char 512)))

  (test
   '(%array (float) 512)
   (walk-type '(¤ (float) 512)))

  (test
   '(%array (const char) 512)
   (walk-type '(¤ (const char) 512)))

  (test
   '(%array (const char))
   (walk-type '(¤ const char)))

  (test
   '(%array (const char))
   (walk-type '(¤ (const char))))

  (test
   '(%array (float) 8)
   (walk-type '(¤ float 8)))

  (test
   "Pointer to const char"
   '(const char *)
   (walk-type '(* const char)))

  (test
   "Const pointer to const char"
   '(const char * const)
   (walk-type '(const * const char)))

  (test
   '(%fun void ((int) (float) (struct what *)))
   (walk-type '(fn ((int) (float) (* struct what)) void)))

  (test
   '(%fun void ((int) (%array (float)) (struct what *)))
   (walk-type '(fn ((int) (¤ float) (* struct what)) void)))

;;; Variable defs
  (test
   '(%var (%array (float) 8) a)
   (walk-var '(var a (¤ float 8))))

  (test
   '(%var (int *) a (& n))
   (walk-var '(var a (* int) (& n))))

  (test
   '(%var (const int *) a (& n))
   (walk-var '(var a (* const int) (& n))))

  (test
   '(%var (struct suc) s)
   (walk-var '(var s (struct suc))))

  (test
   '(%var (const struct suc *) s s1)
   (walk-var '(var s (* const struct suc) s1)))

  (test
   '(%var (const struct suc *) s s1)
   (walk-var '(var s (* (const struct suc)) s1)))

  (test
   '(%var (struct suc) s (hoge piyo))
   (walk-var '(var s (struct suc) (hoge piyo))))

  (test
   '(%var (struct suc *) s (hoge piyo))
   (walk-var '(var s (* struct suc) (hoge piyo))))

;;; Fn defs
  (test
   '(%fun void puk ((int) (%array (float) 8)))
   (walk-fn-def '(fn puk ((int) (¤ float 8)) void)))

  (test
   '(%fun int main ((int argc) ((%array (const char)) argv))
          (return 0))
   (walk-fn-def
    '(fn main ((argc int) (argv (¤ const char))) int
         (return 0))))
  (test
   '(%fun int quxu (((struct piq *) bar))
          (return 0))
   (walk-fn-def
    '(fn quxu ((bar (* struct piq))) int
         (return 0))))

  (test
   '(%fun int quxu (((struct piq *) bar) ((%array (struct tt *)) ploop))
          (return 0))
   (walk-fn-def
    '(fn quxu ((bar (* struct piq)) (ploop (¤ * struct tt))) int
         (return 0))))

  ;; Structs
  (test
   '(struct no_kebab ((int a) (float f)))
   (walk-struct '(struct no-kebab ((a int) (f float)))))

  (test
   '(struct settings ((u32 x y w h)
                      ((%array (struct ((float r g b a))) 4) colors)))
   (walk-struct
    '(struct settings
             ((x y w h u32)
              (colors [¤ struct ((r g b a float)) 4])))))

  (test
   '(struct settings ((u32 x y w h)
                      ((%array (struct color ((float r g b a))) 4) colors)))
   (walk-struct
    '(struct settings
             ((x y w h u32)
              (colors [¤ struct color ((r g b a float)) 4])))))

  (test
   '(struct mega_kebab ((int a)
                        ((struct ((int year) (int month) (int day))) dob)
                        ((%fun int ((int) (%array (int)))) min)))
   (walk-struct '(struct mega-kebab
                         ((a int)
                          (dob (struct ((year int)
                                        (month int)
                                        (day int))))
                          (min (fn ((int) (¤ int)) bool)))))))
