; Programming Languages: Homework 1
; Matthew Iandoli (mjiandoli)
; April 2, 2020

#lang plai-typed

; Problem #1

; Sums a list of numbers
(define (sum [lstNum : (listof number)]) : number
    (foldl + 0 lstNum)
)

(test (sum (list 1 2 3)) 6)
(test (sum empty) 0)
(test (sum (list -1 2 5 0 -3)) 3)

; Problem #2

; Sums a list of numbers whose values are between -5 and -2 (inclusively)
(define (sum-inrange [lstNum : (listof number)]) : number
    (foldl + 0 (filter (lambda ([val : number]) (and (>= val -5) (<= val -2))) lstNum))
)

(test (sum-inrange (list -4 -3 -2)) -9)
(test (sum-inrange (list -5 -3 -4)) -12)
(test (sum-inrange (list -3 -2 -1 3 4 5 -5)) -10)
(test (sum-inrange empty) 0)

; Problem #3

; Checks if the first string starts with the second string
(define (startswith? [strTotal : string] [strStart : string]) : boolean
    (if (> (string-length strStart) (string-length strTotal))
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

; Replaces all strings in a list starting with 'P' with "none"
(define (replaceP [lstStr : (listof string)]) : (listof string)
    (map (lambda ([str : string]) (if (string=? (substring str 0 1) "P") "none" str)) lstStr)
)

(test (replaceP (list "Potatoes" "Tomatoes" "Dill")) (list "none" "Tomatoes" "Dill"))
(test (replaceP (list "nPotatoes" "Tomatoes" "Dill")) (list "nPotatoes" "Tomatoes" "Dill"))
(test (replaceP (list "potatoes" "Tomatoes" "Dill")) (list "potatoes" "Tomatoes" "Dill"))
(test (replaceP empty) empty)

; Problem 5

; Removes all even (and zero) index elements from a list
(define (alternating [lstElems : (listof 'a)]) : (listof 'a)
    (alternatingHelper lstElems #t (list))
)

(define (alternatingHelper [lstElems : (listof 'a)] [alt : boolean] lstResult) : (listof 'a)
    (cond
        [(empty? lstElems) lstResult]
        [(cons? lstElems) (alternatingHelper (rest lstElems) (not alt) (if alt lstResult (append lstResult (list (first lstElems)))))]
    )
)

(test (alternating (list 1 2 3 4)) (list 2 4))
(test (alternating (list "hi" "there" "mom")) (list "there"))
(test (alternating empty) empty)

; Problem 6
(define-type Scores
    [scores
        (hwScore1 : number) ; non-negative
        (hwScore2 : number) ; non-negative
        (hwScore3 : number) ; non-negative
        (hasExtraPoints : boolean)
    ]
)

(define-type Student
    [undergrad
        (name : string)
        (grades : Scores)
        (gradYear : number)
    ]
    [graduate
        (name : string)
        (grades : Scores)
        (degProgram : string) ; "MS" or "PhD"
    ]
)

; Problem 7

; Updates the hasExtraPoints boolean of any student with all grades above 80
(define (assign-points [lstStudents : (listof Student)]) : (listof Student)
    (map
        (lambda ([s : Student])
            (type-case Student s
                [undergrad (n g y) (undergrad n (upgradeScores g) y)]
                [graduate (n g d) (graduate n (upgradeScores g) d)]
            )
        )
        lstStudents
    )
)

(define (upgradeScores [g : Scores]) : Scores
    (scores
        (scores-hwScore1 g)
        (scores-hwScore2 g)
        (scores-hwScore3 g)
        (> (min (min (scores-hwScore1 g) (scores-hwScore2 g)) (scores-hwScore3 g)) 80)
    )
)

(test (assign-points (list (undergrad "Matt" (scores 100 95 90 #f) 2021) (graduate "Bob" (scores 90 85 80 #f) "MS"))) (list (undergrad "Matt" (scores 100 95 90 #t) 2021) (graduate "Bob" (scores 90 85 80 #f) "MS")))

; Problem 8

; Determines if all PhD students have extra points
(define (all-phd-haveextra? [lstStudents : (listof Student)]) : boolean
    (foldl
        (lambda ([b : boolean] [extra : boolean])
            (and b extra)
        )
        #t
        (map
            (lambda ([s : Student])
                (type-case Student s
                    [undergrad (n g y) #t]
                    [graduate (n g d) (or (string=? d "MS") (scores-hasExtraPoints g))]
                )
            )
            lstStudents
        )
    )
)

(define lstGrad1 (list (graduate "Joe" (scores 100 100 90 #f) "PhD") (graduate "Dan" (scores 80 95 90 #f) "MS") (graduate "Julia" (scores 83 92 98 #f) "PhD") (undergrad "Matt" (scores 75 80 90 #f) 2021)))
(define lstGrad2 (list (graduate "Joe" (scores 100 70 90 #f) "PhD") (graduate "Dan" (scores 85 95 90 #f) "MS") (graduate "Julia" (scores 83 92 98 #f) "PhD") (undergrad "Matt" (scores 75 80 90 #f) 2021)))

(test (all-phd-haveextra? (assign-points lstGrad1)) #t)
(test (all-phd-haveextra? (assign-points lstGrad2)) #f)

; Problem 9

; Finds the average rainfall from a list of numbers that follow -999
(define (rainfall [lstRain : (listof number)]) : number
    (rainfallHelper lstRain 0 0 #f)
)

(define (rainfallHelper [lstRain : (listof number)] [sum : number] [count : number] [flag : boolean]) : number
    (cond
        [(empty? lstRain)
            (if (> count 0)
                (/ sum count)
                0
            )
        ]
        [(cons? lstRain)
            (if (not flag)
                (rainfallHelper (rest lstRain) sum count (= (first lstRain) -999))
                (if (>= (first lstRain) 0)
                    (rainfallHelper (rest lstRain) (+ sum (first lstRain)) (+ count 1) flag)
                    (rainfallHelper (rest lstRain) sum count flag)
                )
            )
        ]
    )
)

(test (rainfall (list 1 2 3 4 5 -999 0.5 0 0.75 1 0.75 0)) 0.5)
(test (rainfall (list 1 2 3 4 5 -999 1.5 -1 0 0.5 -2 1 1)) 0.8)
(test (rainfall (list 1 2 3 4 5)) 0)

; Problem 10
(define-type CartItem
    [item
        (name : string)
        (price : number)
    ]
)

; Produces the total cost of a shopping cart (list of items)
; (i) if the cart contains at least $100 worth of hats, take 20% off the cost of all hats (match only items whose exact name is "hat")
; (ii) if the cart contains at least two shoes, take $10 off the total of the cart (match only items whose exact name is "shoes")
(define (checkout [cart : (listof CartItem)]) : number
    (checkoutHelper cart 0 0 0)
)

(define (checkoutHelper [cart : (listof CartItem)] [sum : number] [hatSum : number] [shoeCount : number]) : number
    (cond
        [(empty? cart) (- (- sum (if (>= shoeCount 2) 10 0)) (* hatSum (if (>= hatSum 100) 0.2 0)))]
        [(cons? cart) (checkoutHelper (rest cart) (+ sum (item-price (first cart))) (+ hatSum (if (string=? (item-name (first cart)) "hat") (item-price (first cart)) 0)) (+ shoeCount (if (string=? (item-name (first cart)) "shoes") 1 0)))]
    )
)

(test (checkout (list (item "hat" 25) (item "bag" 50) (item "hat" 85) (item "shoes" 15))) 153)
(test (checkout (list (item "hat" 15) (item "shoes" 75) (item "hat" 25) (item "shoes" 60))) 165)
