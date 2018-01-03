
;; name  	- Tamir Moshiashvili
;; id 	 	- 316131259
;; group number - 05
;; username 	- moshiat

;; helper functions from class

(define (filter p ls)
	(if (null? ls) ls
		(let (
				(hd (car ls))
				(rest (cdr ls))
			)
			(if (p hd) (cons hd (filter p rest)) (filter p rest))
		)
	)
)

(define (reduce binFunc u)
	(define (reduce-helper ls)
		(if (null? ls) u
			(binFunc (car ls) (reduce-helper (cdr ls)))
		)
	)
	reduce-helper
)

;; ----------------------------------------------- PART 1 -----------------------------------------------

;; ends-with - check if str edns with suffix
(define (ends-with suffix str)
	;; starts-with-lists - get short list and long list
	;; check if the long list starts with the variables of the short list
	(define (starts-with-lists short long)
		(cond	
			((null? short) #t)	;; base
			((null? long) #f)	;; the 'long' list need to be longer than 'short'
			((char=? (car short) (car long)) (starts-with-lists (cdr short) (cdr long)))
			(else #f)
		)
	)
	;; str-to-list - make list out of str and reverse it
	(define (str-to-list-reverse str)
		(reverse (string->list str))
	)
	
	;; return value
	(starts-with-lists (str-to-list-reverse suffix) (str-to-list-reverse str))
)

;; mul-of-pairs - suffix is a string, ls is a list of pairs (string, number)
;; multiply every number in ls such that its equivalent string ends with suffix
(define (mul-of-pairs suffix ls)
	;; mul-valid-pairs get a list of pairs (string, number) and return the dot product on the numbers
	(define (mul-valid-pairs ls)
		(cond ((null? ls) 1)	;; base - empty list
			(else (* (cdar ls) (mul-valid-pairs (cdr ls))))	;; (current number) * rest
		)
	)
	
	;; the filter will extract only the pairs that their strings ends with the suffix
	(mul-valid-pairs (filter (lambda (pair) (ends-with suffix (car pair))) ls))
)

;; merge - get two lists and merges between them
(define (merge ls1 ls2)
	;; index 0 - next item from ls1
	;; index 1 - next item from ls2
	(define (concat-next ls1 ls2 index)
		(cond
			((and (null? ls1) (null? ls2)) (list))	;; base - both empty
			((and (= index 0) (null? ls1)) (concat-next ls1 ls2 1))	;; ls1 is empty, continue with ls2
			((and (= index 1) (null? ls2)) (concat-next ls1 ls2 0))	;; ls2 is empty, continue with ls1
			((= index 0) (cons (car ls1) (concat-next (cdr ls1) ls2 1)))
			(else (cons (car ls2) (concat-next ls1 (cdr ls2) 0)))	;; index = 1
		)
	)
	
	;; return value
	(concat-next ls1 ls2 0)
)

;; returns m mod n
(define (mod m n)
	(if (>= m n) (mod (- m n) n)
		m
	)
)

;; rotate ls according to number n clockwise
(define (rotate ls n)
	;; helper functions
	
	;; enumerate - create list of pairs indexing every item in the list
	;;	curr_index - at first call will be the start index
	(define (enumerate ls curr_index)
		(if (null? ls) (list)	;; base - empty list
			;; else
			(cons (cons curr_index (car ls)) (enumerate (cdr ls) (+ curr_index 1)))
		)
	)
	
	(define (undo-enumerate ls)
		(map (lambda (pair) (cdr pair)) ls)
	)
	
	;; body
	(if (null? ls) ls	;; empty list
		;; else
		(let*
			(
				(enumerated (enumerate ls 0))
				(index (- (length ls) (mod n (length ls))))	;; index to switch from
				(before_i_ls (filter (lambda (pair) (< (car pair) index)) enumerated))
				(after_i_ls  (filter (lambda (pair) (>= (car pair) index)) enumerated))
			)
	
			(append (undo-enumerate after_i_ls) (undo-enumerate before_i_ls))
		)
	)
)

;; get comp, which is compare-function, and return a quicksort function
(define (quicksort comp)
	;; quicksort function according to the given comparator
	(define (sort_by_comp ls)
		(if (null? ls) ls	;; base - empty list
			;; else
			(let*
				(
					(curr (car ls))	;; choose the first item as a pivot
					(lower_ls (filter (lambda (x) (< (comp x curr) 0)) ls))
					(greater_ls (filter (lambda (x) (> (comp x curr) 0)) ls))
					(equal_ls (filter (lambda (x) (= (comp x curr) 0)) ls))
				)
				
				(append (append (sort_by_comp lower_ls) equal_ls) (sort_by_comp greater_ls))
			)
		)
	)
	
	sort_by_comp
)

;; ----------------------------------------------- PART 2 -----------------------------------------------

;; helper functions for part 2:

;; represents generic sequence
;; item is the start-item of the sequence
;; head is a function that gets single parameter and returns an item
;; next is a function that gets single parameter and returns new generic sequence
(define (generic_seq item head next)
	(list item head next)
)

;; functions of generic_seq - getters
(define (get_item some_seq) (list-ref some_seq 0))
(define (get_head_function some_seq) (list-ref some_seq 1))
(define (get_next_function some_seq) (list-ref some_seq 2))

(define (hd some_seq) ((get_head_function some_seq) (get_item some_seq)))	;; returns head(item)

(define (tail some_seq)	;; returns new sequence
	(generic_seq
		((get_next_function some_seq) (get_item some_seq))	;; new start item is next(item)
		(get_head_function some_seq)	;; head function of tail sequence is the same function
		(get_next_function some_seq)	;; next function of tail sequence is the same function
	)
)

(define (id_function n) n)	;; f(x) = x

;; the seq function is a generic_seq with n as starting-item, head-function is the id function, next-function is (n+1)
(define (seq n) (generic_seq n id_function (lambda(x) (+ x 1))))

;; the seq-gen function is the same as generic_seq with id_function as head-function
(define (seq-gen n g) (generic_seq n id_function g))

;; the cyclic-seq is a generic_seq, where its item is the list ls,
;;	the head-function is the first item in the list,
;;	the next-function is the rotate-function where the cycle is 1
(define (cyclic-seq ls) (generic_seq ls (lambda (ls) (car ls)) (lambda (ls) (rotate ls (- (length ls) 1)))))

;; ----------------------------------------------- PART 3 -----------------------------------------------

;; helper functions

;; pairs is a list of pairs, each pair is (string, number)
;; key is a string
;; returns pairs[key]
(define (get_val pairs key)
	(let
		;; pair_of_key will be a list of single pair (key, number)
		((pair_of_key (filter (lambda (pair) (string=? (car pair) key)) pairs)))
		;; body
		(if (null? pair_of_key) pair_of_key
			;; else
			(cdar pair_of_key)
		)
	)
)

;; if pair.key is not in pairs.keys, add pair to pairs
;; else pairs[pair.key] = pair.value
(define (insert_pair pairs pair)
	(let
		;; get a list of pairs without the pair which has the same key as the key of the given pair
		((pairs_without_given_pair (filter (lambda (x) (if (string=? (car pair) (car x)) #f #t)) pairs)))
		;; body
		(append (list pair) pairs_without_given_pair)
	)
)

;; creates empty dictionary
(define (make-dictionary)
	;; function of dictionary with its list of pairs (key, value)
	(define (dict ls)
		(lambda (item)
			(cond
				((null? item) ls)	;; returns the list of pairs
				((pair? item) (dict (insert_pair ls item)))	;; return new dictionary with the item
				;; else item is a string
				(else (get_val ls item))	;; returns ls[item]
			)
		)
	)
	
	;; return value
	(dict (list))
)

(load "test.scm")

