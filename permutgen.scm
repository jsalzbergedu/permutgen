;; This creates a lewis structure object with slots for top, bot, left, right, and center
(require-extension coops)
(require-extension coops-primitive-objects)
(require-extension srfi-13)

(define-class element-some-base ()
  ((element-name reader: get-element-name)
   (number-of-dots reader: get-number-of-dots)))

;; Getting from number-of-dots to the position of the dots

(define (number->tlist n)
  (let n-to-tlist ((counter n) (l '()))
  (if (= counter 0)
      l
      (n-to-tlist (- counter 1) (cons #t l)))))

(define (number->flist n)
  (let n-to-flist ((counter n) (l '()))
  (if (= counter 0)
      l
      (n-to-flist (- counter 1) (cons #f l)))))

(define (number->clockwise-boolean-list n)
  (let* ((tlist (number->tlist n))
	 (init-flist (number->flist (- 8 (length tlist))))
	 (init-combined (flatten (zip tlist init-flist)))
	 (flist (number->flist (- 8 (length init-combined)))))
    (append init-combined flist)))

(define (group-in-pairs lst)
  (reverse
   (let g-i-p ((l lst) (acc '()))
     (if (null? l)
	 acc
	 (g-i-p (cddr l) (cons (list (car l) (cadr l)) acc))))))

(define-class element-some ()
  ((element-some-base reader: get-element-some-base)
   (etop reader: get-etop)
   (elef reader: get-elef)
   (erig reader: get-erig)
   (ebot reader: get-ebot)))

(define-method (get-element-name (obj element-some))
  (get-element-name (get-element-some-base obj)))

(define-method (get-number-of-dots (obj element-some))
  (get-number-of-dots (get-element-some-base obj)))

(define (make-element-some-base e-n n-d)
  (make element-some-base 'element-name e-n 'number-of-dots n-d))

(define (decorate-element-some-base e-some-base)
  (let ((epairs (group-in-pairs
		 (number->clockwise-boolean-list
		   (get-number-of-dots e-some-base)))))
    (make element-some 'element-some-base e-some-base 'etop (car epairs) 'erig (cadr epairs) 'ebot (caddr epairs) 'elef (cadddr epairs))))

(define (make-element-some element-name number-of-dots)
  (decorate-element-some-base (make element-some-base 'element-name element-name 'number-of-dots number-of-dots)))

(define-class element-ligand ()
  ((element-some reader: get-element-some)
   (nbonds reader: get-nbonds)))

(define-method (get-etop (obj element-ligand))
  (get-etop (get-element-some obj)))

(define-method (get-ebot (obj element-ligand))
  (get-ebot (get-element-some obj)))

(define-method (get-elef (obj element-ligand))
  (get-elef (get-element-some obj)))

(define-method (get-erig (obj element-ligand))
  (get-erig (get-element-some obj)))

(define-method (get-element-name (obj element-ligand))
  (get-element-name (get-element-some obj)))

(define-method (get-number-of-dots (obj element-ligand))
  (get-number-of-dots (get-element-some obj)))

(define (make-element-ligand element-name number-of-dots nbonds)
  (make element-ligand 'element-some (make-element-some element-name number-of-dots) 'nbonds nbonds))

(define (unwrap x) (x (lambda (some) some) (lambda () '())))

(define-syntax element ; three functions with the syntax of one!
  (syntax-rules ()
    ((_)
     (lambda (some none) (none)))
    ((_ element-name number-of-dots)
     (lambda (some none)
       (some (make-element-some element-name number-of-dots))))
    ((_ element-name number-of-dots nbonds)
     (lambda (some none)
       (some (make-element-ligand element-name number-of-dots nbonds))))))

;; Now as for defining the recursive "4 pronged" lewis structure
(define-class lewis-base ()
  ((element-some reader: get-element-some) ;; the center, which should be an instance of element-some
   (element-top reader: get-element-top)
   (element-bot reader: get-element-bot)
   (element-lef reader: get-element-lef)
   (element-rig reader: get-element-rig)))

(define-class element-with-distance ()
  ((element-ligand reader: get-element-ligand)
   (x reader: get-x)
   (y reader: get-y)))

(define-method (get-etop (obj element-with-distance))
  (get-etop (get-element-ligand obj)))

(define-method (get-ebot (obj element-with-distance))
  (get-ebot (get-element-ligand obj)))

(define-method (get-elef (obj element-with-distance))
  (get-elef (get-element-ligand obj)))

(define-method (get-erig (obj element-with-distance))
  (get-erig (get-element-ligand obj)))

(define-method (get-element-name (obj element-with-distance))
  (get-element-name (get-element-ligand obj)))

(define-method (get-number-of-dots (obj element-with-distance))
  (get-number-of-dots (get-element-ligand obj)))

(define (element-ligand->element-with-distance element-ligand x y)
  (make element-with-distance 'element-ligand element-ligand 'x x 'y y))

(define-class element-with-distance-enumerated ()
  (element-with-distance reader: get-element-with-distance)
  (etop-enumerated reader: get-etop-enumerated)
  (ebot-enumerated reader: get-ebot-enumerated)
  (elef-enumerated reader: get-elef-enumerated)
  (erig-enumerated reader: get-erig-enumerated))

(define-method (get-etop (obj element-with-distance))
  (get-etop (get-element-with-distance obj)))

(define-method (get-ebot (obj element-with-distance))
  (get-ebot (get-element-element-with-distacne obj)))

(define-method (get-elef (obj element-with-distance))
  (get-elef (get-element-with-distance obj)))

(define-method (get-erig (obj element-with-distance))
  (get-erig (get-element-with-distance obj)))

(define-method (get-element-name (obj element-with-distance))
  (get-element-name (get-element-with-distance obj)))

(define-method (get-number-of-dots (obj element-with-distance))
  (get-number-of-dots (get-element-ligand obj)))

(define (make-element-with-distance-enumerated element-with-distance etop-enumerated ebot-enumerated elef-enumerated erig-enumerated)
  (make element-with-distance-enumerated 'element-with-distance element-with-distance
	'etop-enumerated etop-enumerated 'ebot-enumerated ebot-enumerated
	'elef-enumerated elef-enumerated 'erig-enumerated erig-enumerated))

(define (delpair-based-on-pair pair-bool pair-two)
  (cond
   ((car (and (car pair-bool) (cdr pair-bool))) pair-two)
   ((car pair-bool) (list (car pair-two)))
   ((cdr pair-bool) (list (cdr pair-two)))))

(define (enumerate-electrons offset)
  (map (lambda (a) (list (+ (car a) offset) (+ (cadr a) offset)))
	  (map (lambda (a) (list a (+ 4 a)))
	       (let for ((i 4) (acc '()))
		 (if (= 0 i)
		     acc
		     (for (- i 1) (cons i acc)))))))

(define (caddddr p)
  (car (cdr (cdddr p))))

(define (make-lewis-base cen top bot lef rig)
  (make lewis-base 'element-some cen 'element-top top 'element-bot bot 'element-lef lef 'element-rig rig))

(define (lewis-base-ligands->lewis-base-ligands-with-distance lewis-base)
  (let ((cen (get-element-some lewis-base))
	(top (get-element-top lewis-base))
	(bot (get-element-bot lewis-base))
	(lef (get-element-lef lewis-base))
	(rig (get-element-rig lewis-base)))
    (let ((top-decorated (top (lambda (element-ligand) (lambda (some none) (some (element-ligand->element-with-distance element-ligand 0 125)))) (lambda () (lambda (some none) (none)))))
	  (bot-decorated (bot (lambda (element-ligand) (lambda (some none) (some (element-ligand->element-with-distance element-ligand 0 -125)))) (lambda () (lambda (some none) (none)))))
	  (lef-decorated (lef (lambda (element-ligand) (lambda (some none) (some (element-ligand->element-with-distance element-ligand -125 0)))) (lambda () (lambda (some none) (none)))))
	  (rig-decorated (rig (lambda (element-ligand) (lambda (some none) (some (element-ligand->element-with-distance element-ligand 125 0)))) (lambda () (lambda (some none) (none))))))
      (make-lewis-base cen top-decorated bot-decorated lef-decorated rig-decorated))))

(define (lewis-base-ligands-with-distance->lewis-base-ligands-enumerated lewis-base)
  (let ((cen (get-element-some lewis-base))
	(top (get-element-top lewis-base))
	(bot (get-element-bot lewis-base))
	(lef (get-element-lef lewis-base))
	(rig (get-element-rig lewis-base)))
    (let* ((cen-enumerated (enumerate-electrons 0))
	   (top-num (+ 1 (get-number-of-dots (unwrap cen))))
	   (top-enumerated (enumerate-electrons top-num))
	   (rig-enumerated (enumerate-electrons (+ 1 (top (lambda (some) (get-number-of-dots some) (lambda () 0)))
	((cen-decorated (lambda (element-with-distance) (lambda (some none)
				     (let ((t (delpair-based-on-pair (get-etop element-with-distance) (car enumerated-electrons)))
					   (r (delpair-based-on-pair (get-erig element-with-distance) (cadr enumerated-electrons)))
					   (b (delpair-based-on-pair (get-ebot element-with-distance) (caddr enumerated-electrons)))
					   (l (delpair-based-on-pair (get-elef element-with-distance) (cadddr enumerated-electrons))))
				       (some (make-element-with-distance-enumerated some t b l r)))))
			 (lambda () (lambda (some none) (none)))
	  (top-decorated (lambda (some)


;; Mock object
(define mock (lewis-base-ligands->lewis-base-ligands-with-distance
	      (make lewis-base 'element-some (element "C" 4) 'element-top (element) 'element-bot (element) 'element-rig (element "O" 6 2) 'element-lef (element "O" 6 2))))


(define (gen-layout-string lewis-base)
    (let ((cen (get-element-some lewis-base))
	  (top (get-element-top lewis-base))
	  (bot (get-element-bot lewis-base))
	  (lef (get-element-lef lewis-base))
	  (rig (get-element-rig lewis-base)))
      (string-append (cen (lambda (some) (string-append (get-element-name some) "\n" (number->string (get-number-of-dots some)) "\n0\n0\n"))
			  (lambda () "")) " HELLO ONE "
		     (top (lambda (some) (string-append (get-element-name some) "\n" (number->string (get-number-of-dots some)) "\n" (number->string (get-x some)) "\n" (number->string (get-y some)) "\n"))
			  (lambda () "")) " HELLO TWO "
		     (bot (lambda (some) (string-append (get-element-name some) "\n" (number->string (get-number-of-dots some)) "\n" (number->string (get-x some)) "\n" (number->string (get-y some)) "\n"))
			  (lambda () "")) "HELLO THREE "
		     (lef (lambda (some) (string-append (get-element-name some) "\n" (number->string (get-number-of-dots some)) "\n" (number->string (get-x some)) "\n" (number->string (get-y some)) "\n"))
			  (lambda () "")) " HELLO FOUR "
		     (rig (lambda (some) (string-append (get-element-name some) "\n" (number->string (get-number-of-dots some)) "\n" (number->string (get-x some)) "\n" (number->string (get-y some)) "\n"))
			  (lambda () "")))))

	  
