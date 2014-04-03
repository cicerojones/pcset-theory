(defun list+ (n lst)
  (mapcar #'(lambda (x) (+ n x)) lst))

(defun Tn1-11 (n pcset)     ;expects an argument of n=12
    (cond ((= n -1) ())
	  (t (cons (list+ n pcset)
		   (Tn1-11 (- n 1) pcset)))))

(defun mod12 (lst)
  (mapcar #'(lambda (x) (mod x 12)) lst))

(defun mod12-all (lst-of-lsts)
  (mapcar #'mod12 lst-of-lsts))

(defun TnI-all (lol-of-pcsets)
 (mapcar #'(lambda (x) (reverse (mod12-all (Tn1-11 11 x)))) lol-of-pcsets))

(defun flatten (x)
  (labels 
      ((rec (x acc) 
	 (cond ((null x) acc) 
	       ((atom x) (cons x acc)) 
	       (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; (defun count-occurrences (vector-list)
;;   (cond ((null vector-list) ())
;; ;; expects only the 12 pitch classes
;; 	(t (list (count 0 vector-list)
;; 		 (count 1 vector-list)
;; 		 (count 2 vector-list)
;; 		 (count 3 vector-list)
;; 		 (count 4 vector-list)
;; 		 (count 5 vector-list)
;; 		 (count 6 vector-list)
;; 		 (count 7 vector-list)
;; 		 (count 8 vector-list)
;; 		 (count 9 vector-list)
;; 		 (count 10 vector-list)
;; 		 (count 11 vector-list)))))

;; this is an exhaustive version of COUNT that is also
;; recursive. why does it need to use LABELS? so you don't
;; have to explicitly pass in '11', or so you can call 
;; REVERSE at the end? Probably possible to write it more simply.

(defun counting (lst-of-12ints)
  "Use COUNT to find the number of occurrences of each
 member of the list"
  (labels ((count-up (n ints)
	     (cond ((< n 0) ())
		   (t (cons (count n ints)
			    (count-up (1- n) ints))))))
    ;; assumes you want to be looking at the twelve pitch-classes
    (reverse (count-up 11 lst-of-12ints))))

(defun repeating (num-element1 num-element2)
  (cond ((= 0 num-element1) ())
	(t (cons num-element2
		 (repeating (- num-element1 1) num-element2)))))
(defun 12-profile (vector)
  (list (repeating (nth 0 vector) 0)
	(repeating (nth 1 vector) 1)
	(repeating (nth 2 vector) 2)
	(repeating (nth 3 vector) 3)
	(repeating (nth 4 vector) 4)
	(repeating (nth 5 vector) 5)
	(repeating (nth 6 vector) 6)
	(repeating (nth 7 vector) 7)
	(repeating (nth 8 vector) 8)
	(repeating (nth 9 vector) 9)
	(repeating (nth 10 vector) 10)
	(repeating (nth 11 vector) 11)))

(defun clean-profile (single-vector)
  (flatten (remove-if #'null (12-profile single-vector))))

(defun de-nestC (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom (car x)) (cons x acc)) 
		   (t (rec (car x) (rec (cdr x) acc)))))) 
    (rec x nil)))

(defun set-up (pcset1 lst-of-pcsets)
  (cond ((null lst-of-pcsets) ())
	(t (cons pcset1 
		 (cons (car lst-of-pcsets) 
		       (set-up pcset1 (cdr lst-of-pcsets)))))))

(defun make-pairs-from-list (lst)
  "turn a list of anythings into pairs of those things, simply
conjoining every two elements"
  (cond ((null lst) ())
	(t (cons (list (first lst) (second lst))
		 (make-pairs-from-list (nthcdr 2 lst))))))

(defun jack-b (listA listA-prime)
  (cond ((null listA-prime) ())
	(t (cons 
	    (list (car listA)
		  (car listA-prime))
	    (jack-b listA (cdr listA-prime))))))

(defun last1 (lst) (car (last lst)))

(defun jack-rec (listA listA-prime)
      (cond ((null listA) ())
            (t (labels ((rec (an-x a-y)
                          (cond ((null listA) ())
                                (t (jack-b an-x a-y)))))
                 (list (rec listA listA-prime) (jack-rec (cdr listA) (cdr listA-prime)))))))



(defun set-up-Tn-equivalent-Ys-with-X (pcset1 pcset2)
  (set-up pcset1 (car (tni-all (list pcset2)))))





(defun pc-compari (n1 n2)
  (cond ((<= n1 n2)
	 (cond ((= n1 n2) 0)
	       (t (1+ (pc-compari n1 (1- n2))))))
	(t (cond ((= (mod n1 12) n2) 0)
	       (t (1+ (pc-compari (1+ n1) n2)))))))

(defun rec-do-pc-compari (lst1 lst2)
  (labels ((rec (x1 y1)
	     (cond ((null x1) ())
		   (t (cons (pc-compari (car x1) (car y1))
			    (rec (cdr x1) y1))))))
    (cond ((null lst2) ())
	  (t (cons (rec lst1 lst2)
		   (rec-do-pc-compari lst1 (cdr lst2)))))))

(defun create-lewin-vector (set1 set2)
  (counting (flatten (rec-do-pc-compari set1 set2))))

(defun create-straus-profile (set1 set2)
  (clean-profile (create-lewin-vector set1 set2)))

(defun create-12-straus-profiles (set1 set2)
  (let ((a (make-pairs-from-list (set-up-Tn-equivalent-Ys-with-X set1 set2))))
    (labels ((rec (lst)
	       (create-straus-profile (first lst) (second lst))))
      (mapcar #'rec a))))

(defun create-12-lewin-vectors (set1 set2)
  (let ((a (make-pairs-from-list (set-up-Tn-equivalent-Ys-with-X set1 set2))))
    (labels ((rec (lst)
	       (create-lewin-vector (first lst) (second lst))))
      (mapcar #'rec a))))

(defun createA-lewin-vector (paired-set)
  (counting (flatten (rec-do-pc-compari (car paired-set) (cadr paired-set)))))

(defun create-24-lewin-vectors (paired-list)
  (let* ((a (make-pairs-from-list 
	     (set-up-Tn-equivalent-Ys-with-X (car paired-list) 
					     (cadr paired-list))))
	 (b (mapcar #'createA-lewin-vector a))
	 (c (mapcar #'reverse b)))
    (append b c)))

(defun create-Astraus-profile (paired-list)
  (clean-profile (createA-lewin-vector paired-list)))

(defun create-12A-straus-profiles (paired-list)
  (let ((a (make-pairs-from-list 
	    (set-up-Tn-equivalent-Ys-with-X (car paired-list) 
					    (cadr paired-list)))))
    (mapcar #'create-Astraus-profile a)))

(defun looking (mset)
  (cond ((null mset) ())
	(t (cons (count (car mset) mset)
		 (looking (cdr mset))))))

(defun comparing (mset-list)
  (let ((canon (mapcar #'list-to-integer (mapcar #'looking mset-list))))
    (list (apply #'max canon) canon)))

(defun locating (mset-list)
  (let ((chart (comparing mset-list)))
    (nth (1- (cadr (locate (car chart) (cadr chart)))) mset-list)))

(defun locate (x lat)
  (let ((newlat (reverse lat)))
    (labels
	((rec (rec-x rec-lat)
	   (cond ((null rec-lat) ())
		 ((equal rec-x (car rec-lat)) 
		  (cons (length rec-lat) (rec rec-x (cdr rec-lat))))
		 (t (rec rec-x (cdr rec-lat))))))
      (cons x (reverse (rec x newlat))))))

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))

(defun my-rotate (lst)
  (let ((n (length lst)))
    (labels 
	((rec (rec-lst rec-n)
	   (if (zerop rec-n) ()
	       (list (rotate rec-lst rec-n)
		     (rec rec-lst (1- rec-n))))))
      (rec lst n))))

(defun all-rotations (lst)
  (butlast (de-nestC (my-rotate lst))))

(defun mod12math (less more)
  (cond ((<= less more) (- more less))
	(t (- (+ 12 more) less))))

(defun primer (ordered-set)
  (cond ((null ordered-set) ())
	(t (cons (mod12math (car ordered-set) (last1 ordered-set))
		 (primer (butlast ordered-set))))))

(defun prime-vector (ordered-set)
  (let ((basic-vector (primer ordered-set)))
    (cons (car basic-vector) (butlast (reverse (butlast basic-vector))))))

(defun wrap-vectors (ordered-set)
  (mapcar #'prime-vector (all-rotations ordered-set)))

(defun list-to-integer (lst-of-ints)
  (let ((high-exp (length lst-of-ints)))
    (labels 
	((rec (lst place)
	   (cond ((null lst) 0)
		 (t (+ (* (car lst) (expt 10 place))
		       (rec (cdr lst) (1- place)))))))
      (rec lst-of-ints high-exp))))

(defun find-prime-form-from-list (ordered-pcset)
  (let* ((a (all-rotations ordered-pcset))
	 (b (mapcar #'prime-vector a))
	 (c (mapcar #'list-to-integer b)))
    (nth (position (apply #'min c) c) a)))

(defun set-to-zero (ordered-pcset)
  (let ((n (car ordered-pcset)))
    (mapcar #'(lambda (x) 
		(cond ((>= x n) (- x n))
		      (t (- (+ 12 x) n))))
	    ordered-pcset)))

(defun invert-mod12-pcset (pcset)
 (sort (mapcan #'mod12 (list (mapcar #'(lambda (x) (- 12 x)) pcset))) #'<))

(defun Ainvert-mod12-pcset (pcset)
 (mapcan #'mod12 (list (mapcar #'(lambda (x) (- 12 x)) pcset))))

(defun find-set-inversion (pcset)
  (set-to-zero (reverse (Ainvert-mod12-pcset pcset))))

(defun forte-decision (pcset)
  (let ((a (set-to-zero (find-prime-form-from-list pcset))))
    (list a (set-to-zero (find-prime-form-from-list (invert-mod12-pcset a))))))

(defun forte-prime (pcset)
  (let ((a (forte-decision pcset)))
    (if (every #'<= (car a) (cadr a))
	(car a)
	(cadr a))))

(defun forte-primeA (pcset)
  (let ((a (forte-decision pcset)))
    (another-forte (car a) (cadr a))))

(defun another-forte (setx sety)
  (let ((a setx) (b sety))
    (labels ((rec (set1 set2)
	       (cond ((null set1) setx)
		     ((> (car set1) (car set2)) sety)
		     ((< (car set1) (car set2)) setx)
		     (t (rec (cdr set1) (cdr set2))))))
      (rec a b))))

;; (defun 24create (paired-list)
;;   (let* ((a (make-pairs-from-list 
;; 	     (set-up-Tn-equivalent-Ys-with-X (car paired-list) 
;; 					     (cadr paired-list))))
;; 	 (b (mapcar #'createA-lewin-vector a))
;; 	 (c (mapcar #'reverse b))
;; 	 (d (append b c))
;; 	 (e (mapcar #'clean-profile d)) ; effectively creates a straus-profile
;; 	 (f (mapcar #'multiset-forte-prime e))
;; 	 (g (mapcar #'multiset-forte-decision e)))
;;     (mapcar #'list d e f g)))

;; simply remove duplicates you say, eh? how would you reintroduce
;; them? quote unquote.
(defun multiset-forte-primeA (mset)
 (forte-primeA (remove-duplicates mset)))

(defun multiset-forte-prime (mset)
 (forte-prime (remove-duplicates mset)))

(defun multiset-forte-decision (mset)
  (forte-decision (remove-duplicates mset)))

(defun imset-decision-maker (paired-list)
  (let* (
	 ;; a is the "home-list", the foundation, providing all the
	 ;; paired sets across the transpositional spectrum for a given
	 ;; X-Y pair
	 (a (make-pairs-from-list 
	     (set-up-Tn-equivalent-Ys-with-X (car paired-list) 
					     (cadr paired-list))))
	 ;; determine the IFUNC of each X-Y pair in A, where the X chord is 
	 ;; constant and Y is transposed by Tn for n from 0 to 11
	 (b (mapcar #'createA-lewin-vector a))
	 ;; also determine the INVERSE of each IFUNC simply
	 ;; at one time by reversing the lewin-vector
	 ;;
	 ;; disadvantage is that you don't get the generating pair
	 ;; explicitly, but still a neat trick on Lewin-style vectors
	 (c (mapcar #'reverse b))
	 ;; make one big list that contains all these
	 ;; lewin-vectors. we will then use this list of 24 IFUNCS in
	 ;; Lewin form to find the preferred prime-form according to the 
	 ;; prime-form algorithm of choice
	 (d (append b c))
	 ;; create the analogous 24 straus-profiles
	 (e (mapcar #'clean-profile d)) ; turns out CLEAN-PROFILE effectively creates a Straus-profile
	 ;; determine the forte prime form that the reduced
	 ;; straus multisets represent
	 ;; 
	 ;; could be replaced with Straus-Rahn algorithm if desired
	 (f (multiset-forte-primeA (car e))) ; car because only need check the first one as they
					     ; all will represent the same forteprime (why, again?)
	 ;; make all imsets into sets in order to perform
	 ;; some nifty LOCATE-ing maneuvers
	 (g (mapcar #'remove-duplicates e))
	 ;; find the location(s) of the forte prime in this 
	 ;; new list of sets
	 ;;
	 ;; NB. F is a shallow list, and contains the only the single prime-form
	 (h (locate f g))
	 ;; IMPORTANT: use the location of 
	 ;; prime-form f in the list of sets g
	 ;; to find the generating multiset parent(s) in e
	 ;; possible that there is MORE THAN ONE!
	 (i (loop for idx in (cdr h) ; must use cdr portion of LOCATE output
	       collect (nth (1- idx) e))) ;locate is not zero-indexed apparently
	 ;; leave only unique representatives generators of the
	 ;; forte-prime form. at max there should be two 
	 ;;
	 ;; NOT NECESSARILY!!! see ((0 2 4) (0 2 6))
	 (j (remove-duplicates i :test #'equal)))
    ;; following code commented out because it failed to 
    ;; detect the prime form in the specific tricky case 
    ;; mentioned just above. otherwise, a hack worth
    ;; looking at, at least for its use of EVERY
    ;;
    ;; (if (every #'<= (car j) (cadr j))
    ;; 	(list 'first j)
    ;; 	(list 'second j))))
    ;;
    ;;; this alone just cannot do it.
    ;;; there must be a larger function for looking 
    ;;; within multiset families--update: that is now
    ;;; done by the function, LOCATING below
    ;; do pairwise comparisons to find the imset that 
    ;; most consistently is lower from left to right
    ;; and use LOCATING in the special case
    ;;
    ;; in compilation this won't print but use for seeing
    ;; step by step the process here
    ;;
    ;; (format t "~{~{~a ~%~}~%~}" (list a d e f g h i j))
    (if (<= (length j) 2)
	(if (every #'<= (car j) (cadr j))
	    (car j)
	    (cadr j))
	(locating j))))

;; (defun help-view-imsets (paired-list) 
;;   (format t "~{~a~%~{~{~28a ~}~%~}~}" (list paired-list (24create paired-list))))
;;
;; neatly print out trichords with INFUNCS
;; may fail for other cardinalities to print 
;; with good column aligning because of the ~28
;; should be adjusted to use global of choice



(defun map-locate (loli)
  (let ((full-list loli))
   (labels ((hmm (rec-loli)
	      (cond ((null rec-loli) ())
		    (t (cons (locate (car rec-loli) full-list)
			     (hmm (cdr rec-loli)))))))
     (hmm loli))))

