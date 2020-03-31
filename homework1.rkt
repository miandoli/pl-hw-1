#lang plai-typed

; Problem #1
(define (sum lstNum)
    (if (empty? lstNum)
        0
        (+ (first lstNum) (sum (rest lstNum)))
    )
)

(test (sum (list 1 2 3)) 6)
(test (sum (list)) 0)
(test (sum (list -1 2 5 0 -3)) 3)

; Problem #2
(define (sum-inrange lstNum)
    (if (empty? lstNum)
        0
        (+ (inrange (first lstNum)) (sum-inrange (rest lstNum)))
    )
)

(define (inrange val)
    (if (and (>= val -5) (<= val -2))
        val
        0
    )
)

(test (sum-inrange (list -4 -3 -2)) -9)
(test (sum-inrange (list -5 -3 -4)) -12)
(test (sum-inrange (list -3 -2 -1 3 4 5 -5)) -10)

; Problem #3
(define (startswith? strTotal strStart
    (if (> (string-length strStart) (string-length strTotal
        #f
        (string=? (substring strTotal 0 (string-length strStart)) strStart)
    )
)

(test (startswith? "Apple" "App") #t)
(test (startswith? "Apple" "app") #f)
(test (startswith? "asdf" "") #t) ; assuming all strings start with the empty string
(test (startswith? "Apple" "Apple") #t)
(test (startswith? "Apple" "some bigger string than the previous") #f)

; Problem 4
(define (replaceP lstStr)
    (map replacePHelper lstStr)
)

(define (replacePHelper str)
    (if (string=? (substring str 0 1) "P")
        "none"
        str
    )
)

(test (replaceP (list "Potatoes" "Tomatoes" "Dill")) (list "none" "Tomatoes" "Dill"))
(test (replaceP (list "nPotatoes" "Tomatoes" "Dill")) (list "nPotatoes" "Tomatoes" "Dill"))
(test (replaceP (list "potatoes" "Tomatoes" "Dill")) (list "potatoes" "Tomatoes" "Dill"))
(test (replaceP (list)) (list))

; Problem 5
(define (alternating lstElems)
    (alternatingHelper lstElems #t (list))
)

(define (alternatingHelper lstElems alt lstResult)
    (if (empty? lstElems)
        lstResult
        (alternatingHelper (rest lstElems) (not alt) (if alt lstResult (append lstResult (list (first lstElems)))))
    )
)

(test (alternating (list 1 2 3 4)) (list 2 4))
(test (alternating (list "hi" "there" "mom")) (list "there"))
(test (alternating (list)) (list))

; Problem 6
(define-type )
