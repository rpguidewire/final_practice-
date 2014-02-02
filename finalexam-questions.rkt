;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname finalexam-questions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Question 1
;; This function produces a list of all even numbers from 0 to input number "num"
(define (make-even-list num)
(filter (lambda (x) (not (equal? x ""))) (build-list num (lambda (x) (cond 
                                                                  [(odd? x) ""]
                                                                  [else x])) )))

;;Question 2
;;The purpose of this function is to use mutual recursion on a tree and find the value associated with the input key, 

;;Question 3
;Data Definition 

;Dt
;empty 
;(cons (list num value) DtList)

;DtList
;empty 
;(cons Dt DtList)

;;Write the Template for the above data definition 
;(define (my-dt-fn dtlist)
;  (cond
;    [(empty? dtlist)....]
;    [....(first (first dtlist))...
;      ....(second (first dtlist))...
;      ...(my-dt-fn (rest dtlist))...]))


;;Question 4
;;Given a string with char and whitespace return the lenght of the longest word 
;;Ex "Hello my name is shivangi" --> 8

(define trialstring "Hello my name is shivangi")

(define (string->tokens string)
  (first (quicksort (map length (list->tokenslst (string->list string))) >)))

(define (list->tokenslst listofchar)
  (cond 
    [(empty? listofchar) empty]
    [else (cons (firsttoken listofchar)
                (list->tokenslst (resttoken (rest listofchar))))]))

(define (firsttoken loc)
  (cond
    [(empty? loc) empty]
    [(char-whitespace? (first loc)) empty]
    [else (cons (first loc);(first loc)
                (firsttoken (rest loc)))]))

(define (resttoken loc)
  (cond
    [(empty? loc) empty]
    [(char-whitespace? (first loc)) (rest loc)]
    [else (resttoken (rest loc))]))

(check-expect (string->tokens trialstring) 8)

;(char-whitespace? "One Two\nThree")
;;string->tokens: (Char->Boolean) String -> (listof String)


;;Question 5

;;Replace the list associated wiht 1 part of hte graph retain the rest of the graph
;;replace the value associated with 1 key in the tree retain the rest of the tree

;;Tree Binary Tree 
(define-struct node (key value l r))
;;A Node = (make-node (Num String Node Node))

(define lef2 (make-node 1 "you" empty empty))
(define lef3 (make-node 4 "today?" empty empty))
(define rig3 (make-node 6 "luc" empty empty))
(define rig1 (make-node 5 "Are" lef3 rig3))
(define lef1 (make-node 2 "How" lef2 empty))
(define master (make-node 3 "Hello" lef1 rig1))

(define (btree-replace key tree new-value)
  (cond 
    [(empty? tree) empty]
    [(> key (node-key tree)) (make-node (node-key tree)
                                        (node-value tree)
                                        (node-l tree)
                                        (btree-replace key (node-r tree) new-value))]
    [(< key (node-key tree)) (make-node (node-key tree)
                                        (node-value tree)
                                        (btree-replace key (node-r tree) new-value)
                                        (node-r tree))]
    [(= key (node-key tree)) (make-node (node-key tree)
                                         new-value
                                         (node-l tree)
                                         (node-r tree))]
    [else false]))

(check-expect (btree-replace 5 master "are") (make-node 3 "Hello"
                                                        (make-node 2 "How" (make-node 1 "you" empty empty) empty)
                                                        (make-node 5 "are" (make-node 4 "today?" empty empty)
                                                                           (make-node 6 "luc" empty empty))))

;;Normal Tree


;;Graph 



;;Question 6 
;;Abstraction of two similar problems 
(define (sum-of-many listofint)
  (cond 
    [(empty? listofint) 0]
    [else (+ (first listofint)
             (sum-of-many (rest listofint)))]))

(define (mul-of-many listofint)
  (cond
    [(empty? listofint) 1]
    [else (* (first listofint)
             (mul-of-many (rest listofint)))]))

;;Data Abstraction 
(define (exp-of-many function base listofint)
  (cond 
    [(empty? listofint) base]
    [else (function (first listofint)
             (exp-of-many function base (rest listofint)))]))

(check-expect (exp-of-many * 1 (list 2 3 4)) 24)

;;Question 7
;;take a less-than two ranges (more/less) creates a function which takes a "transform" function and applies it to every item in 
;; in the input list which is between the said ranges 

;;Version 1
(define (map-in-range function val-less val-more transform lst)
(local 
  [(define (in-range? function value-less value-more)
     (filter (lambda (x) (and (function value-more x) (function x value-less ))) lst))]
  (map (lambda (x) (transform x)) (in-range? function val-less val-more)))) 

;;Version2 --> Correct One!!! 
(define (mapping-in-range function more-val less-val)
  (lambda (transform lst) (map transform (filter (lambda (x) (and (function more-val x)
                                                                  (function x less-val ))) lst))))

(check-expect ((mapping-in-range string<? "apple" "dog") string-length (list "ball" "kite" "super")) (list 4))

;((mapping-in-range string<? "Apple" "Hat") string-length (list "ball" "kite" "super"))

;;Question 8
;;Tree/Graph both write a function that traces through the tree to look for a key lists all the places it has visited in a list
;;format the ending two should be the key and the value/list attached 

;;Question 9
;;Reachable given a graph find out which node on the graph had been visited or to-do thish still have to be done based on the module
;;graph 
