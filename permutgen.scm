;; This creates a lewis structure object with slots for top, bot, left, right, and center
(require-extension coops)
(require-extension srfi-13)
(require-extension irregex)
(use parley)

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
  (cond
   ((> n 4)
    (flatten (zip (number->tlist 4) (append (number->tlist (- n 4)) (number->flist (- 8 n))))))
   ((= n 4)
    (flatten (zip (number->tlist 4) (number->flist 4))))
   ((< n 4)
    (flatten (zip (append (number->tlist n) (number->flist (- 4 n))) (number->flist 4))))))
      

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

(define-method (get-nbonds (obj element-with-distance))
  (get-nbonds (get-element-ligand obj)))

(define (element-ligand->element-with-distance element-ligand x y)
  (make element-with-distance 'element-ligand element-ligand 'x x 'y y))

(define-class element-with-distance-enumerated ()
  ((element-with-distance reader: get-element-with-distance)
  (etop-enumerated reader: get-etop-enumerated)
  (ebot-enumerated reader: get-ebot-enumerated)
  (elef-enumerated reader: get-elef-enumerated)
  (erig-enumerated reader: get-erig-enumerated)))

(define-method (get-element-ligand (obj element-with-distance-enumerated))
  (get-element-ligand (get-element-with-distance obj)))

(define-method (get-etop (obj element-with-distance-enumerated))
  (get-etop (get-element-with-distance obj)))

(define-method (get-ebot (obj element-with-distance-enumerated))
  (get-ebot (get-element-with-distance obj)))

(define-method (get-elef (obj element-with-distance-enumerated))
  (get-elef (get-element-with-distance obj)))

(define-method (get-erig (obj element-with-distance-enumerated))
  (get-erig (get-element-with-distance obj)))

(define-method (get-element-name (obj element-with-distance-enumerated))
  (get-element-name (get-element-with-distance obj)))

(define-method (get-number-of-dots (obj element-with-distance-enumerated))
  (get-number-of-dots (get-element-ligand obj)))

(define-method (get-x (obj element-with-distance-enumerated))
  (get-x (get-element-with-distance obj)))

(define-method (get-y (obj element-with-distance-enumerated))
  (get-y (get-element-with-distance obj)))

(define-method (get-nbonds (obj element-with-distance-enumerated))
			   (get-nbonds (get-element-with-distance obj)))

(define (make-element-with-distance-enumerated element-with-distance etop-enumerated ebot-enumerated elef-enumerated erig-enumerated)
  (make element-with-distance-enumerated 'element-with-distance element-with-distance
	'etop-enumerated etop-enumerated 'ebot-enumerated ebot-enumerated
	'elef-enumerated elef-enumerated 'erig-enumerated erig-enumerated))

(define (delpair-based-on-pair pair-bool pair-two)
  (let ((tag (gensym)))
    (filter (lambda (x) (not (equal? x tag)))
	    (map (lambda (bool x) (if bool x tag))
		 pair-bool pair-two))))

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
    (let ((top-decorated (top (lambda (element-ligand)
				(lambda (some none) (some (element-ligand->element-with-distance
						      element-ligand 0 125))))
			      (lambda () (lambda (some none) (none)))))
	  (bot-decorated (bot (lambda (element-ligand)
				(lambda (some none) (some (element-ligand->element-with-distance
						      element-ligand 0 -125))))
			      (lambda () (lambda (some none) (none)))))
	  (lef-decorated (lef (lambda (element-ligand) (lambda (some none) (some
								  (element-ligand->element-with-distance
								   element-ligand -125 0))))
			      (lambda () (lambda (some none) (none)))))
	  (rig-decorated (rig (lambda (element-ligand) (lambda (some none)
						    (some (element-ligand->element-with-distance
							   element-ligand 125 0))))
			      (lambda () (lambda (some none) (none))))))
      (make-lewis-base cen top-decorated bot-decorated lef-decorated rig-decorated))))

(define (element-with-distance->element-with-distance-enumerated element-with-distance lst)
  (element-with-distance
   (lambda (element-with-distance)
     (lambda (some none)
       (let ((t (delpair-based-on-pair (get-etop element-with-distance) (car lst)))
	     (r (delpair-based-on-pair (get-erig element-with-distance) (cadr lst)))
	     (b (delpair-based-on-pair (get-ebot element-with-distance) (caddr lst)))
	     (l (delpair-based-on-pair (get-elef element-with-distance) (cadddr lst))))
	 (some (make-element-with-distance-enumerated element-with-distance t b l r)))))
   (lambda () (lambda (some none) (none)))))

(define-class element-some-enumerated ()
  ((element-some reader: get-element-some)
  (etop-enumerated reader: get-etop-enumerated)
  (ebot-enumerated reader: get-ebot-enumerated)
  (elef-enumerated reader: get-elef-enumerated)
  (erig-enumerated reader: get-erig-enumerated)))

(define-method (get-element-name (obj element-some-enumerated))
  (get-element-name (get-element-some obj)))

(define-method (get-number-of-dots (obj element-some-enumerated))
  (get-number-of-dots (get-element-some obj)))

(define-method (get-etop (obj element-some-enumerated))
  (get-etop (get-element-some obj)))

(define-method (get-ebot (obj element-some-enumerated))
  (get-ebot (get-element-some obj)))

(define-method (get-elef (obj element-some-enumerated))
  (get-elef (get-element-some obj)))

(define-method (get-erig (obj element-some-enumerated))
  (get-erig (get-element-some obj)))

(define (make-element-some-enumerated element-some etop-enumerated
				      ebot-enumerated elef-enumerated erig-enumerated)
  (make element-some-enumerated 'element-some element-some
	'etop-enumerated etop-enumerated 'ebot-enumerated ebot-enumerated
	'elef-enumerated elef-enumerated 'erig-enumerated erig-enumerated))

(define (element-some->element-some-enumerated element-some lst)
  (element-some
   (lambda (element-some-some)
     (lambda (some none)
       (let ((t (delpair-based-on-pair (get-etop element-some-some) (car lst)))
	     (r (delpair-based-on-pair (get-erig element-some-some) (cadr lst)))
	     (b (delpair-based-on-pair (get-ebot element-some-some) (caddr lst)))
	     (l (delpair-based-on-pair (get-elef element-some-some) (cadddr lst))))
	 (some (make-element-some-enumerated element-some-some t b l r)))))
   (lambda () (lambda (some none) (none)))))

(define (lewis-base-ligands-with-distance->lewis-base-ligands-enumerated lewis-base)
  (let ((cen (get-element-some lewis-base))
	(top (get-element-top lewis-base))
	(bot (get-element-bot lewis-base))
	(lef (get-element-lef lewis-base))
	(rig (get-element-rig lewis-base)))
    (let* ((cen-enumerated (enumerate-electrons 0))
	   (cen-offset (+ 1 (get-number-of-dots (unwrap cen))))
	   (top-enumerated (enumerate-electrons cen-offset))
	   (top-offset (top (lambda (some) (+ 1 (get-number-of-dots some) cen-offset)) (lambda () cen-offset)))
	   (rig-enumerated (enumerate-electrons top-offset))
	   (rig-offset (rig (lambda (some) (+ 1 (get-number-of-dots some) top-offset)) (lambda () top-offset)))
	   (bot-enumerated (enumerate-electrons rig-offset))
	   (bot-offset (bot (lambda (some) (+ 1 (get-number-of-dots some) rig-offset)) (lambda () rig-offset)))
	   (lef-enumerated (enumerate-electrons bot-offset))
	   (cen-decorated (element-some->element-some-enumerated cen cen-enumerated))
	   (top-decorated (element-with-distance->element-with-distance-enumerated top top-enumerated))
	   (rig-decorated (element-with-distance->element-with-distance-enumerated rig rig-enumerated))
	   (bot-decorated (element-with-distance->element-with-distance-enumerated bot bot-enumerated))
	   (lef-decorated (element-with-distance->element-with-distance-enumerated lef lef-enumerated)))
      (make-lewis-base cen-decorated top-decorated bot-decorated lef-decorated rig-decorated))))

;; Mock object
(define mock (lewis-base-ligands-with-distance->lewis-base-ligands-enumerated
	      (lewis-base-ligands->lewis-base-ligands-with-distance
	       (make lewis-base 'element-some (element "C" 4) 'element-top (element) 'element-bot (element) 'element-rig (element "O" 6 2) 'element-lef (element "O" 6 2)))))


(define (gen-layout-string lewis-base)
    (let ((cen (get-element-some lewis-base))
	  (top (get-element-top lewis-base))
	  (bot (get-element-bot lewis-base))
	  (lef (get-element-lef lewis-base))
	  (rig (get-element-rig lewis-base)))
      (string-append
       (cen (lambda (some) (string-append (get-element-name some) "\n"
				     (number->string (get-number-of-dots some)) "\n0\n0\n"))
	    (lambda () ""))
       (top (lambda (some) (string-append (get-element-name some) "\n"
				     (number->string (get-number-of-dots some)) "\n"
				     (number->string (get-x some)) "\n"
				     (number->string (get-y some)) "\n"))
	    (lambda () ""))
       (rig (lambda (some) (string-append (get-element-name some) "\n"
				     (number->string (get-number-of-dots some)) "\n"
				     (number->string (get-x some)) "\n"
				     (number->string (get-y some)) "\n"))
	    (lambda () ""))
       (bot (lambda (some) (string-append (get-element-name some) "\n"
				     (number->string (get-number-of-dots some)) "\n"
				     (number->string (get-x some)) "\n"
				     (number->string (get-y some)) "\n"))
	    (lambda () ""))
       (lef (lambda (some) (string-append (get-element-name some) "\n"
				     (number->string (get-number-of-dots some)) "\n"
				     (number->string (get-x some)) "\n"
				     (number->string (get-y some)) "\n"))
	    (lambda () "")))))

(define (getall-electron-numbers-element-some element-some-enumerated)
  (element-some-enumerated (lambda (some)
			     (let ((top (get-etop-enumerated some))
				   (rig (get-erig-enumerated some))
				   (bot (get-ebot-enumerated some))
				   (lef (get-elef-enumerated some)))
			       (flatten (list top rig bot lef))))
			   (lambda () (list))))

(define (each-sender-to-receiver senders-lst receivers-lst)
  (let e-s-t-r ((s-list senders-lst) (r-list receivers-lst) (acc '()))
    (if (null? s-list)
	(flatten acc)
	(let* ((fst-s-list (car s-list))
	       (s-to-r (flatten (map (lambda (a) (list fst-s-list a)) r-list))))
	  (e-s-t-r (cdr s-list) r-list (cons s-to-r acc))))))

(define cen-dir 0)
(define top-dir 1)
(define rig-dir 2)
(define bot-dir 3)
(define lef-dir 4)

(define (unwrap-enumerated-electrons element dir)
  (cond 
   ((= dir top-dir)
    (element (lambda (some) (get-etop-enumerated some)) (lambda () #f)))
   ((= dir rig-dir)
    (element (lambda (some) (get-erig-enumerated some)) (lambda () #f)))
   ((= dir bot-dir)
    (element (lambda (some) (get-ebot-enumerated some)) (lambda () #f)))
   ((= dir lef-dir)
    (element (lambda (some) (get-elef-enumerated some)) (lambda () #f)))))

(define (permutgen-first-level lewis-base)
  (let ((cen (get-element-some lewis-base))
	(top (get-element-top lewis-base))
	(bot (get-element-bot lewis-base))
	(lef (get-element-lef lewis-base))
	(rig (get-element-rig lewis-base)))
    (let ((top-nbonds (top (lambda (some) (get-nbonds some)) (lambda () #f)))
	  (bot-nbonds (bot (lambda (some) (get-nbonds some)) (lambda () #f)))
	  (lef-nbonds (lef (lambda (some) (get-nbonds some)) (lambda () #f)))
	  (rig-nbonds (rig (lambda (some) (get-nbonds some)) (lambda () #f))))
      (let ((top-senders (if top-nbonds (getall-electron-numbers-element-some top) #f))
	    (bot-senders (if bot-nbonds (getall-electron-numbers-element-some bot) #f))
	    (lef-senders (if lef-nbonds (getall-electron-numbers-element-some lef) #f))
	    (rig-senders (if rig-nbonds (getall-electron-numbers-element-some rig) #f))
	    (cen-revievers (getall-electron-numbers-element-some cen)))
	  (let ((cen-top-enumerated (unwrap-enumerated-electrons cen top-dir))
		(cen-bot-enumerated (unwrap-enumerated-electrons cen bot-dir))
		(cen-lef-enumerated (unwrap-enumerated-electrons cen lef-dir))
		(cen-rig-enumerated (unwrap-enumerated-electrons cen rig-dir)))
	    (flatten (list (if (and top-senders cen-top-enumerated)
			       (each-sender-to-receiver top-senders cen-top-enumerated) '())
			   (if (and bot-senders cen-bot-enumerated)
			       (each-sender-to-receiver bot-senders cen-bot-enumerated) '())
			   (if (and lef-senders cen-lef-enumerated)
			       (each-sender-to-receiver lef-senders cen-lef-enumerated) '())
			   (if (and rig-senders cen-rig-enumerated)
			       (each-sender-to-receiver rig-senders cen-rig-enumerated) '()))))))))

(define (gen-verts lewis-base)
  (let ((cen (get-element-some lewis-base))
	(top (get-element-top lewis-base))
	(bot (get-element-bot lewis-base)))
    (let ((cen-top-enumerated (unwrap-enumerated-electrons cen top-dir))
	  (cen-bot-enumerated (unwrap-enumerated-electrons cen bot-dir))
	  (top-bot-enumerated (unwrap-enumerated-electrons top bot-dir))
	  (bot-top-enumerated (unwrap-enumerated-electrons bot top-dir)))
      (flatten (list (if (and cen-top-enumerated top-bot-enumerated)
			 (list (each-sender-to-receiver cen-top-enumerated top-bot-enumerated)
			       (each-sender-to-receiver top-bot-enumerated cen-top-enumerated))
			 '())
		     (if (and cen-bot-enumerated bot-top-enumerated)
			 (list (each-sender-to-receiver cen-top-enumerated bot-top-enumerated)
			       (each-sender-to-receiver bot-top-enumerated cen-top-enumerated))
			 '()))))))

(define (nlist->ret-sep-list lst)
  (if (null? lst)
      ""
      (foldr string-append "" (map (lambda (n) (string-append (number->string n) "\n")) lst))))

(define (make-level lewis-base)
  (string-append "#This level's element initlist begins\n"
		 (gen-layout-string lewis-base)
		 "~~~\n"
		 "#This level's permutations of electron combinations begins\n"
		 (nlist->ret-sep-list (permutgen-first-level lewis-base))
		 "\n"
		 "~~~\n"
		 "#This level's list of vertical bonds begins\n"
		 (nlist->ret-sep-list (gen-verts mock))
		 "~~~\n"))

(define (get-level-file file)
  (call-with-input-file file (lambda (in) (read-lines in))))

(define (parse-line-of-input-file line)
  (and (>= (string-length line) 7)
	 (and (string= (substring line 0 7) "element")
		(let ((split (irregex-split " " line)))
		  (cond ((= 4 (length split))
			 (element (cadr split) (string->number (caddr split))
				  (string->number (cadddr split))))
			((= 3 (length split))
			 (element (cadr split) (string->number (caddr split))))
			((= 1 (length split))
			 (element))
			(else #f))))))

(define make-lewis-base-curried
  (lambda (cen) (lambda (top) (lambda (bot) (lambda (lef) (lambda (rig)
					(make-lewis-base cen top bot lef rig)))))))

(define (funcallif proc . arg)
  (if (car arg)
      (apply proc arg)
      proc))

(define (lines->lewis-base-list lines)
  (let r-t-l ((lns lines) (proc make-lewis-base-curried) (acc '()))
    ;; the code for getting a lewis-base from 5 lines of the level file
    (if (procedure? proc)
	(r-t-l (cdr lns) (let ((parsed (parse-line-of-input-file (car lns))))
			   (if parsed
			       (proc parsed)
			       proc)) acc)
	(if (null? lns)
	    (reverse (cons proc acc))
	    (r-t-l lns make-lewis-base-curried (cons proc acc))))))

(define (lewis-base->level lewis-base)
  (make-level
   (lewis-base-ligands-with-distance->lewis-base-ligands-enumerated
    (lewis-base-ligands->lewis-base-ligands-with-distance
     lewis-base))))

(define (lewis-base-list->levels lst)
  (foldr string-append "" (map lewis-base->level lst)))

(define file->levels file
  (lines->lewis-base-list (get-level-file file)))

(unless (irregex-search "csi" (car (argv)))
  (define infile (parley "Input file name: "))
  (define outfile (parley "Output file name: "))
  (delete-file outfile)
  (define level (lewis-base-list->levels (lines->lewis-base-list (get-level-file infile))))
  (with-output-to-file outfile (lambda () (display level))))
