;;;; Some code from "Paradigms of Artificial Intelligence Programming"
;;; https://norvig.github.io/paip-lisp

#+murmel
(progn
(load "mlib")
(defmacro first (l) `(car ,l))
(defmacro rest (l) `(cdr ,l))

(defmacro defparameter (s v . d)
  `(define ,s ,v))
(defmacro defvar (s v . d)
  `(define ,s ,v))
)


;; 1.7 Higher-Order Functions
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))


;; 2.2 A Straightforward Solution
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

; test:
; (sentence)  ; => (THE WOMAN HIT THE BALL)

; (trace 'sentence 'noun-phrase 'verb-phrase 'article 'noun 'verb)
; => (SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB)

; test:
; > (sentence) =>
; (1 ENTER SENTENCE)
;   (1 ENTER NOUN-PHRASE)
;     (1 ENTER ARTICLE)
;     (1 EXIT ARTICLE: (THE))
;     (1 ENTER NOUN)
;     (1 EXIT NOUN: (MAN))
;   (1 EXIT NOUN-PHRASE: (THE MAN))
;   (1 ENTER VERB-PHRASE)
;     (1 ENTER VERB)
;     (1 EXIT VERB: (HIT))
;     (1 ENTER NOUN-PHRASE)
;       (1 ENTER ARTICLE)
;       (1 EXIT ARTICLE: (THE))
;       (1 ENTER NOUN)
;       (1 EXIT NOUN: (BALL))
;     (1 EXIT NOUN-PHRASE: (THE BALL))
;   (1 EXIT VERB-PHRASE: (HIT THE BALL))
; (1 EXIT SENTENCE: (THE MAN HIT THE BALL))
; (THE MAN HIT THE BALL)

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

; test:
; (sentence) ; => (THE WOMAN HIT THE BALL)
; (sentence) ; => (THE WOMAN HIT THE MAN)
; (sentence) ; => (THE BALL SAW THE WOMAN)
; (sentence) ; => (THE BALL SAW THE TABLE)
; (noun-phrase) ; => (THE MAN)
; (verb-phrase) ; => (LIKED THE WOMAN)


;; 2.3 A Rule-Based Solution
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

; test:
; (assoc 'noun *grammar*) ; => (NOUN -> MAN BALL WOMAN TABLE)

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

; test:
; (generate 'sentence) ; => (THE TABLE SAW THE BALL)
; (generate 'sentence) ; => (THE WOMAN HIT A TABLE)
; (generate 'noun-phrase) ; => (THE MAN)
; (generate 'verb-phrase) ; => (TOOK A TABLE)


;; 2.5 Changing the Grammar without Changing the Program
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

; (generate 'sentence)
; => (A TABLE ON A TABLE IN THE BLUE ADIABATIC MAN SAW ROBIN
;    WITH A LITTLE WOMAN)


;; 2.6 Using the Same Data for Several Programs
(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

; test:
; (pprint (generate-tree 'Sentence))
; => (SENTENCE (NOUN-PHRASE (ARTICLE A)
;                           (ADJ*)
;                           (NOUN WOMAN)
;                           (PP*))
;          (VERB-PHRASE (VERB HIT)
;                           (NOUN-PHRASE (PRONOUN HE))
;                           (PP*)))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

;; switch back to *simple-grammar*.
;; PAIP says that generate-all will stackoverflow with *bigger-grammar*
(setf *grammar* *simple-grammar*)

; test:
; (generate-all 'Article) ; => ((THE) (A))
; (generate-all 'Noun) ; => ((MAN) (BALL) (WOMAN) (TABLE))

; (generate-all 'noun-phrase)
; => ((A MAN) (A BALL) (A WOMAN) (A TABLE)
;     (THE MAN) (THE BALL) (THE WOMAN) (THE TABLE))

 (length (generate-all 'sentence)) ; => 256
