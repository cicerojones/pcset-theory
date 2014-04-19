;;;; A suite of functions for dealing with a new, relatively-esoteric
;;;; concept in the relatively esoteric world of post-tonal music
;;;; theory: the interval multiset (or imset), which is derived from
;;;; the concept of an 'interval function' (or IFUNC) as described by
;;;; influential music theorist David Lewin in his classic Generalized
;;;; Musical Intervals and Transformations

;;;; The following code is being used to provide the computational
;;;; underpinning for research to be published soon in a scholarly
;;;; music theory journal and, as such, is not quite ready for public
;;;; consumption. There is a considerable amount of commenting that
;;;; exists mostly to keep a kind of running note-taking for me as I
;;;; consider the issues of design and how to adjust to the changing
;;;; needs of the data which is being produced for the different
;;;; structures being inputted. the commenting style needs to be
;;;; adjusted so more of the useful information there appears in a
;;;; documentation string (most functions lack such documentation).
;;;; Consider also providing consistent appearances of examples, cf.
;;;; the highly-structured model used in How To Design Programs

;;;; While there is a very rough attempt to group functions that
;;;; belong together in the same sections, consider a more
;;;; standardized approach to semi-colon commenting. for some reason,
;;;; hash-pipe commenting blocks seems to be glitchy in Aquamacs, but is
;;;; otherwise quite useful

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;begin;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; initialize globals that hold pitch-class sets as laid out
;;; according to the Forte-system. Uses lists of pitch classes rather
;;; than strings or vectors with no whitespace, largely because of Lispy
;;; propensities. Could probably be re-formatted if performance 
;;; ever became an issue. As it is, compiling this whole file populates 
;;; the necessary databases in decent enough time, despite reliance
;;; on inefficient data structures and some inevitably sloppy coding (still
;;; not bad for my first big Lisp program, I must say).

;;tn-types

;; temporary fix for loading data as a file
(load "/Users/a/Documents/starting-directory/projects/T3L-programming/artistic\ language\ related/music/Lisp/interval-function\ related/imset-independent-project-files-for-pima/refactoring-data.lisp")

;; the massive concatenation of all 215 (?) Tn set-types
(defvar *tn-types* 
  (append *dyads-tn* *trichords-tn* *tetrachords-tn* *pentachords-tn* *hexachords-tn*))
(defvar *tni-types* (append *dyads* *trichords* *tetrachords* *pentachords* *hexachords*))

;;; global variables that will receive particular pairings of the
;;; above pc-sets

;; for example, in the case of the setting the global variable
;; *all-trichord-pairs* several lines below, SET-UP is to produce 19 *
;; 19 = 361 pairs of X-Y trichord pc-sets, for use in determining the
;; IFUNC(X,Y) as per David Lewin's Generalized Musical Intervals and
;; Transformations

(defvar *all-dyad-pairs* ())
(defvar *all-trichord-pairs* ())
(defvar *all-tetrachord-pairs* ())
(defvar *all-pentachord-pairs* ())
(defvar *all-hexachord-pairs* ())

;; pairs of mixed-cardinality sets
(defvar *2x3pairs* ())
(defvar *2x4pairs* ())
(defvar *2x5pairs* ())
(defvar *2x6pairs* ())
(defvar *3x4pairs* ())
(defvar *3x5pairs* ())
(defvar *3x6pairs* ())
(defvar *4x5pairs* ())
(defvar *4x6pairs* ())
(defvar *5x6pairs* ())
(defvar *all-mixed-pairs* ())
(defvar *all-pairs* ())
(defvar *all-pairs-flat* ())

;; including TnI-equivalent sets
(defvar *all-dyad-pairs-TNI* ())
(defvar *all-trichord-pairs-TNI* ())
(defvar *all-tetrachord-pairs-TNI* ())
(defvar *all-pentachord-pairs-TNI* ())
(defvar *all-hexachord-pairs-TNI* ())

;; pairs of mixed-cardinality, TnI-equivalent sets
(defvar *2x3pairs-TNI* ())
(defvar *2x4pairs-TNI* ())
(defvar *2x5pairs-TNI* ())
(defvar *2x6pairs-TNI* ())
(defvar *3x4pairs-TNI* ())
(defvar *3x5pairs-TNI* ())
(defvar *3x6pairs-TNI* ())
(defvar *4x5pairs-TNI* ())
(defvar *4x6pairs-TNI* ())
(defvar *5x6pairs-TNI* ())

;; more significant data-stores
(defvar *all-mixed-pairs-TNI* ())
(defvar *all-pairs-TNI* ())
(defvar *all-pairs-flat-TNI* ())
(defvar *locatable* ())
(defvar *imsets* ())
(defvar *imsets-no-dups* ())
(defvar *sub-im* ())
(defvar *ordered-im-redux* ())
(defvar *x-yabi* ())

;;; various functions for setting up databases of X-Y pairs

;; return a list to a state of nesting often desired in IFUNC-computation
;; note: there must be a built-in that does the same thing
(defun de-nestC (lol)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom (car x)) (cons x acc)) ; if the thing at the head of the list is an is list with an atom than add that structure to the acc
		   (t (rec (car x) (rec (cdr x) acc)))))) ; huh
    (rec lol nil)))

;; cons pcset1 onto a list featuring
;; each one of the sets in the lst-of-pcsets
(defun set-up (pcset1 lst-of-pcsets)
  (cond ((null lst-of-pcsets) ())
	(t (cons pcset1 
		 (cons (car lst-of-pcsets) 
		       (set-up pcset1 (cdr lst-of-pcsets)))))))

;; perform this set-up of a single base-set across all the trichords,
;; inversions included, as is uses tn-types
(defun set-up-all (base-set)
  (append (set-up base-set *dyads-tn*)
	  (set-up base-set *trichords-tn*)
	  (set-up base-set *tetrachords-tn*)
	  (set-up base-set *pentachords-tn*)
	  (set-up base-set *hexachords-tn*)))

;; for the straighforward case of sets of the same cardinality
(defun pair-x-y-forms-same-cardinality (base-set)
  (cond ((= 2 (length base-set)) 
	 (make-pairs-from-list (set-up base-set *dyads-tn*)))
	((= 3 (length base-set)) 
	 (make-pairs-from-list (set-up base-set *trichords-tn*)))
	((= 4 (length base-set)) 
	 (make-pairs-from-list (set-up base-set *tetrachords-tn*)))
	((= 5 (length base-set)) 
	 (make-pairs-from-list (set-up base-set *pentachords-tn*)))
	((= 6 (length base-set))
	 (make-pairs-from-list (set-up base-set *hexachords-tn*)))))

;; general-purpose (i.e. not musical set theory specific) code

(defun make-pairs-from-list (lst)
  "turn a list of anythings into pairs of those things, simply
conjoining every two elements"
  (cond ((null lst) ())
	(t (cons (list (first lst) (second lst))
		 (make-pairs-from-list (nthcdr 2 lst))))))

;; mostly a wrapper for the two above functions
(defun pair-all-x-y-forms (base-set)
  (make-pairs-from-list (set-up-all base-set)))

;; create the various databases
(setf *all-dyad-pairs* (mapcar #'pair-x-y-forms-same-cardinality *dyads-tn*))
(setf *all-trichord-pairs* (mapcar #'pair-x-y-forms-same-cardinality *trichords-tn*))
(setf *all-tetrachord-pairs* (mapcar #'pair-x-y-forms-same-cardinality *tetrachords-tn*))
(setf *all-pentachord-pairs* (mapcar #'pair-x-y-forms-same-cardinality *pentachords-tn*))
(setf *all-hexachord-pairs* (mapcar #'pair-x-y-forms-same-cardinality *hexachords-tn*))
;; compare nesting of
(setf *2X3PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *trichords-tn*)) *dyads-tn*)))
(setf *2X4PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *tetrachords-tn*)) *dyads-tn*)))
(setf *2X5PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *pentachords-tn*)) *dyads-tn*)))
(setf *2X6PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords-tn*)) *dyads-tn*)))
(setf *3X4PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *tetrachords-tn*)) *trichords-tn*)))
(setf *3X5PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *pentachords-tn*)) *trichords-tn*)))
(setf *3X6PAIRS* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords-tn*)) *trichords-tn*)))
(setf *4X5PAIRS*  (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *pentachords-tn*)) *tetrachords-tn*)))
(setf *4X6PAIRS*  (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords-tn*)) *tetrachords-tn*)))
(setf *5X6PAIRS*  (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords-tn*)) *pentachords-tn*)))

;; produces the preferred nesting
(setf *all-dyad-pairs* (make-pairs-from-list (de-nestc *all-dyad-pairs*)))
(setf *all-trichord-pairs* (make-pairs-from-list (de-nestc *all-trichord-pairs*)))
(setf *all-tetrachord-pairs* (make-pairs-from-list (de-nestc *all-tetrachord-pairs*)))
(setf *all-pentachord-pairs* (make-pairs-from-list (de-nestc *all-pentachord-pairs*)))
(setf *all-hexachord-pairs* (make-pairs-from-list (de-nestc *all-hexachord-pairs*)))

    
(SETF *ALL-MIXED-PAIRS*
      (append *2X3PAIRS*
	      *2X4PAIRS*
	      *2X5PAIRS*
	      *2X6PAIRS*
	      *3X4PAIRS*
	      *3X5PAIRS*
	      *3X6PAIRS*
	      *4X5PAIRS*
	      *4X6PAIRS*
	      *5X6PAIRS*))

(setf *all-pairs*
      (list *all-dyad-pairs*
	    *2X3PAIRS*
	    *2X4PAIRS*
	    *all-trichord-pairs*
	    *2X5PAIRS*
	    *2X6PAIRS*
	    *3X4PAIRS*
	    *3X5PAIRS*
	    *all-tetrachord-pairs*
	    *3X6PAIRS*
	    *4X5PAIRS*
	    *4X6PAIRS*
	    *all-pentachord-pairs*
	    *5X6PAIRS*
	    *all-hexachord-pairs*))

;;
(defun jack-b (listA listA-prime)
  (cond ((null listA-prime) ())
	(t (cons 
	    (list (car listA)
		  (car listA-prime))
	    (jack-b listA (cdr listA-prime))))))

(defun jack-rec (listA listA-prime)
      (cond ((null listA) ())
            (t (labels ((rec (an-x a-y)
                          (cond ((null listA) ())
                                (t (jack-b an-x a-y)))))
                 (list (rec listA listA-prime) (jack-rec (cdr listA) (cdr listA-prime)))))))

;;TnI-types
(setf *all-dyad-pairs-TNI* (butlast (make-pairs-from-list (de-nestc (jack-rec *dyads* *dyads*)))))
(setf *all-trichord-pairs-TNI* (butlast (make-pairs-from-list (de-nestc (jack-rec *trichords* *trichords*)))))
(setf *all-tetrachord-pairs-TNI* (butlast (make-pairs-from-list (de-nestc (jack-rec *tetrachords* *tetrachords*)))))
(setf *all-pentachord-pairs-TNI* (butlast (make-pairs-from-list (de-nestc (jack-rec *pentachords* *pentachords*)))))
(setf *all-hexachord-pairs-TNI* (butlast (make-pairs-from-list (de-nestc (jack-rec *hexachords* *hexachords*)))))
;; compare nesting of
(setf *2X3PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *trichords*)) *dyads*)))
(setf *2X4PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *tetrachords*)) *dyads*)))
(setf *2X5PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *pentachords*)) *dyads*)))
(setf *2X6PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords*)) *dyads*)))
(setf *3X4PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *tetrachords*)) *trichords*)))
(setf *3X5PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *pentachords*)) *trichords*)))
(setf *3X6PAIRS-TNI* (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords*)) *trichords*)))
(setf *4X5PAIRS-TNI*  (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *pentachords*)) *tetrachords*)))
(setf *4X6PAIRS-TNI*  (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords*)) *tetrachords*)))
(setf *5X6PAIRS-TNI*  (make-pairs-from-list (mapcan #'(lambda (x) (set-up x *hexachords*)) *pentachords*)))

;; produces the preferred nesting
(setf *all-dyad-pairs-TNI* (make-pairs-from-list (de-nestc *all-dyad-pairs-TNI*)))
(setf *all-trichord-pairs-TNI* (make-pairs-from-list (de-nestc *all-trichord-pairs-TNI*)))
(setf *all-tetrachord-pairs-TNI* (make-pairs-from-list (de-nestc *all-tetrachord-pairs-TNI*)))
(setf *all-pentachord-pairs-TNI* (make-pairs-from-list (de-nestc *all-pentachord-pairs-TNI*)))
(setf *all-hexachord-pairs-TNI* (make-pairs-from-list (de-nestc *all-hexachord-pairs-TNI*)))

(setf *all-pairs-flat-TNI*
      (append *all-dyad-pairs-TNI*
	      *2X3PAIRS-TNI*
	      *2X4PAIRS-TNI*
	      *all-trichord-pairs-TNI*
	      *2X5PAIRS-TNI*
	      *2X6PAIRS-TNI*
	      *3X4PAIRS-TNI*
	      *3X5PAIRS-TNI*
	      *all-tetrachord-pairs-TNI*
	      *3X6PAIRS-TNI*
	      *4X5PAIRS-TNI*
	      *4X6PAIRS-TNI*
	      *all-pentachord-pairs-TNI*
	      *5X6PAIRS-TNI*
	      *all-hexachord-pairs-TNI*))


;; note how this was reversed to be the preferred order as above! get your code
;; to write your code for you!
;;
;; CL-USER> `(setf *all-mixed-pairs* ,(cons 'append (reverse (quote (*5X6PAIRS* *4X6PAIRS* *4X5PAIRS* *3X6PAIRS* *3X5PAIRS* *3X4PAIRS* *2X6PAIRS* *2X5PAIRS* *2X4PAIRS* *2X3PAIRS*)))))
;; (SETF *ALL-MIXED-PAIRS* (APPEND *2X3PAIRS* *2X4PAIRS* *2X5PAIRS* *2X6PAIRS* *3X4PAIRS* *3X5PAIRS* *3X6PAIRS* *4X5PAIRS* *4X6PAIRS* *5X6PAIRS*))


;;; functions for computing IFUNC

;; utilities for working with lists, specifically of pitch-classes

(defun list+ (n lst)
  (mapcar #'(lambda (x) (+ n x)) lst))

(defun last1 (lst) (car (last lst)))

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

;; Paul Graham's FLATTEN function
(defun flatten (x)
  (labels 
      ((rec (x acc) 
	 (cond ((null x) acc) 
	       ((atom x) (cons x acc)) 
	       (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil))) the trichord-global
;;
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



(setf *locatable* (loop for pair in *all-pairs-flat-TNI*
		       collect (imset-decision-maker pair)))
(setf *imsets* (map-locate *locatable*))
(setf *imsets-no-dups* (remove-duplicates *imsets* :test #'equal :key #'car))
(setf *sub-im* (loop for entry in *imsets-no-dups* 
                 collect (cons (car entry)
                               (loop for idx in (cdr entry) 
				  collect (elt *all-pairs-flat-TNI* (1- idx))))))

;; will this compile? yes!
(setf *ordered-im-redux* (sort *sub-im* #'< :key #'(lambda (x) (list-to-integer (car x)))))

;; if so, this will print it out
;; (format t "~{~{~a~%~}~%~}" *ordered-im-redux*)
