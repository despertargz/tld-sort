#lang racket

(define (recombine-tlds parts) 	
  (cond 
    ((null? parts) '())
    ((null? (cdr parts)) (list (car parts)))
    ((string=? (car (cdr parts)) "uk")
     (cons (string-append (car parts) (car (cdr parts))) (recombine-tlds (cdr (cdr parts)))))
    (else (cons (car parts) (recombine-tlds (cdr parts))))))

(define (compare-domain-parts? a-parts b-parts)
  (cond
    ((and (null? a-parts) (null? b-parts)) #f)
    ((null? a-parts) #t)
    ((null? b-parts) #f)
    ((string=? (car a-parts) (car b-parts)) (compare-domain-parts? (cdr a-parts) (cdr b-parts)))
    (else (string<? (car a-parts) (car b-parts)))))
  
(define (compare-domains a b)
  (compare-domain-parts?
   (reverse (recombine-tlds (string-split a ".")))
   (reverse (recombine-tlds (string-split b ".")))))

(display-lines-to-file
 (sort (file->lines "domains-in.txt") compare-domains)
 "domains-out.txt")
