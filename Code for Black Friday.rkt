#lang racket

;CS Project: Do certain groups spend more on Black Friday?
;Project by: Anjali Jha, Dechen Khangkyil, Prajita Niraula, Reina Shahi 

(require csc151)
(require plot)

;Random sample for data-set of approximately 550,000 cases
(define blackfridayfile
  (read-csv-file "C:/Users/reina/Desktop/CSC151-Lab/BlackFriday1000.csv"))

;;; Procedure:
;;;   list-ref-by-indices
;;; Parameters:
;;;   table, a list
;;;   column-indices, a list of integers
;;; Purpose:
;;;   to select the value(s) in a table of the position of the value(s) in column-indices
;;; Produces:
;;;   column-requested
(define list-ref-by-indices
  (lambda (table column-indices)
    (map (section list-ref table <>) column-indices))) ; Citation: Shahi, Reina. Exam 2. CSC 151-01. 2018 Fall 


;;; Procedure:
;;;   select-colums
;;; Parameters:
;;;   table, a list of lists of lists
;;;   column-indices, a list of integers
;;; Purpose:
;;;   to select the column(s) in a table of the position of the value(s) in column-indices
;;; Produces:
;;;   selected-columns, a list of lists of lists
;;; Preconditions:
;;;   column-indices should not have a value that extends (- (length lst) 1)
;;; Postconditions:
;;;   selected-columns should only display (length column-indices) values in each   innermost list
;;;   selected-columns should display the columns of the position of the value(s) in column-indices
;;;     for example, > (select-columns (list '("apple" 2 5) '("banana" 3 5)) (list 0 2))
;;;                  '(("apple" 5) ("banana" 5))                         
(define select-columns
  (lambda (table column-indices)
    (map (section list-ref-by-indices <> column-indices) table))) ; Citation: Shahi, Reina. Exam 2. CSC 151-01. 2018 Fall


;--------------------------------------------------------------------
; 1. CLEANING OUR DATA

;;; Procedure:
;;;  remove-last-char  
;;; Parameters:
;;;  str, a string  
;;; Purpose:
;;;  remove the last char from the string 
;;; Produces:
;;;; result, a string
(define remove-last-char
  (lambda (str)
    (list->string (reverse (drop (reverse (string->list str)) 1)))))


; Final, cleaned data
(define final-data
  (drop (map reverse
             (map cons
                  (map (o string->number (section remove-last-char <>) car) (select-columns blackfridayfile (list 8)))
                  (map reverse (select-columns blackfridayfile (iota 7))))) 1))


;-----------------------------------------------------------------------------
;2. GENERIC CODE TO PLOT VARIOUS INFORMATION OF THE AMOUNT PURCHASED OF DIFFERENT CATEGORICAL VARIABLES

;;; Procedure:
;;;  plot-category
;;; Parameters:
;;;  list-index, a column index 
;;;  str, a string 
;;; Purpose:
;;;  depending upon str shows either the total, mean, maximum, minimum or median revenue for categories in the column given by list-index 
;;; Produces:
;;;;  plotted-gram, a histogram 
;;; Preconditions:
;;;  str should be either "mean", "total", "maximum", "minimum" or "median"
;;;  list-index should be smaller than 7 
;;; Postconditions:
;;;  * if str = "mean", a histogram showing mean revenue for each category of a variable given by list-index (in the heterogeneous list final-data) is plotted
;;;  * if str = "total", a histogram showing total revenue for each category of a variable given by list-index (in the heterogeneous list final-data) is plotted
;;;  * if str = "maximum", a histogram showing maximum revenue for each category of a variable given by list-index (in the heterogeneous list final-data) is plotted
;;;  * if str = "minimum", a histogram showing minimum revenue for each category of a variable given by list-index (in the heterogeneous list final-data) is plotted
;;;  * if str = "median", a histogram showing median revenue for each category of a variable given by list-index (in the heterogeneous list final-data) is plotted

(define plot-category   
  (lambda (list-index str)
    (let* ([categories-list (map car (tally-all (apply append (select-columns final-data (list list-index)))))]
           [category-values-list (lambda (category) (sort (map cadr (filter (o (section equal? <> category) car) (select-columns final-data (list list-index 7)))) <))]
           [proc-mean (lambda (category) (list category  (/ (reduce + (category-values-list category)) (length (category-values-list category)))))]
           [proc-total (lambda (category) (list category  (reduce + (category-values-list category))))]
           [proc-max (lambda (category) (list category  (apply max (category-values-list category))))]
           [proc-min (lambda (category) (list category  (apply min (category-values-list category))))]
           [proc-median (lambda (category)(list category (list-ref (category-values-list category) (round (/(length (category-values-list category))2)))))])
           
      (cond
        [(string-ci=? str "mean") 
         (plot (discrete-histogram (map (section proc-mean <>) categories-list)))]
        [(string-ci=? str "total") 
         (plot (discrete-histogram (map (section proc-total <>) categories-list)))]
        [(string-ci=? str "maximum")
         (plot (discrete-histogram (map (section proc-max <>) categories-list)))]
        [(string-ci=? str "minimum")
         (plot (discrete-histogram (map (section proc-min <>) categories-list)))]
        [(string-ci=? str "median")
         (plot (discrete-histogram (map (section proc-median <>) categories-list)))]
        [else 
         #f])))) 

;------------------------------------------------------------
; 3. Z-TESTS AND T-TESTS

; ONE-SAMPLE-Z-TEST

;;; Procedure:
;;;  one-sample-z-test
;;; Parameters:
;;;  total, a list of 2 numbers
;;;  props, a list of 2 lists 
;;; Purpose:
;;;  to find the z-stat of a given proportion that is to undergo a hypothesis test
;;; Produces:
;;;  z-stat, a real number 
;;; Preconditions:
;;;  * total should be in the format '(total-number-of-category-1 total-number-of-category-2)
;;;  * props should be in the format '(("category1" proportion-of-category-1)
;;;                                    ("category2" proportion-of-category-2)) 
;;; Postconditions:
;;;  z-stat must be the resulting z-statistics
;;;  https://www.statisticshowto.datasciencecentral.com/probability-and-statistics/z-score/
(define one-sample-z-test
  (lambda (total props)
      (let* ([sd (sqrt (/ (* 0.5 0.5) (car total)))])
        (/ (- (cadr (car props)) 0.5) sd))))


;List of total numbers of males and females in sample data
(define f-and-m-total
  (map cadr (tally-all (select-columns final-data (list 2)))))

;Proportion of females and males in dataset
(define f-and-m-prop
  (map cons (list "F" "M") (map list (map / (map cadr (tally-all (map caddr final-data))) (make-list 2 (length final-data))))))

;List of total numbers of married and unmarried individuals in sample data
(define marital-status-total
  (map cadr (tally-all (select-columns final-data (list 6)))))

;Proportion of married and umarried in dataset
(define ms-prop
  (map cons (list "Married" "Single") (map list (map / (map cadr (tally-all (map (section list-ref <> 6) final-data))) (make-list 2 (length final-data))))))



; TWO-SAMPLE-T-TEST

;;; Procedure:
;;;  two-sample-t-test
;;; Parameters:
;;;  total, a list of 2 numbers 
;;;  means, a list of 2 lists
;;;  sds, a list of 2 lists
;;; Purpose:
;;;  find the t-stat of the difference of two means to find the p-value of the hypothesis test 
;;; Produces:
;;;  t-stat, a real number
;;; Preconditions:
;;;  * total should be in the format '(total-number-of-category-1 total-number-of-category-2)
;;;  * means should be in the format '(("category 1" mean-of-category-1)
;;;                                    ("category 2" mean-of-category-2))
;;;  * sds should be in the format '(("category 1" standard-deviation-of-category-1)
;;;                                  ("category 2" standard-deviation-of-category-2))
;;; Postconditions:
;;;  t-stat must be the resulting t-statistic
;;;  https://www.statisticshowto.datasciencecentral.com/t-statistic/
(define two-sample-t-test
  (lambda (total means sds) 
      (let ([combined-sd (sqrt (+ (/ (square (cadr (car sds))) (car total))
                                  (/ (square (cadr (cadr sds))) (cadr total))))])
        (/ (- (cadr (car means)) (cadr (cadr means))) combined-sd))))

; List with sex category and amount purchased of each case in blackfridayfile
(define f-and-m-purchase
  (select-columns final-data (list 2 7)))
 
; List with means of amount purchased of sex categories
(define f-and-m-mean-purchase
  (let calculate-mean ([remaining-data f-and-m-purchase] 
                       [sum-female 0]
                       [sum-male 0])
    (let* ([total-f-and-m (map cadr (tally-all (map caddr final-data)))])
      (cond [(null? remaining-data)
             (map cons (list "F" "M") (map list (list (round (/ sum-female (car total-f-and-m))) (round (/ sum-male (cadr total-f-and-m))))))]
            [(equal? "F" (car (car remaining-data)))
             (calculate-mean (cdr remaining-data) (+ (cadr (car remaining-data)) sum-female) sum-male)]
            [else
             (calculate-mean (cdr remaining-data) sum-female (+ (cadr (car remaining-data)) sum-male))]))))


; List with standard deviations of amount purchased sex categories
(define f-and-m-sd
  (let calculate-sd ([remaining-data f-and-m-purchase]
                     [sum-of-square-f 0]
                     [sum-of-square-m 0])
    (let* ([total-f-and-m (map cadr (tally-all (map caddr final-data)))])
      (cond [(null? remaining-data)
             (map cons (list "F" "M") (map list (list (round (sqrt (/ sum-of-square-f (- (car total-f-and-m) 1))))
                                                      (round (sqrt (/ sum-of-square-m (- (cadr total-f-and-m) 1)))))))]
            [(equal? "F" (car (car remaining-data)))
             (calculate-sd
              (cdr remaining-data)
              (+ (square (- (cadr (car f-and-m-mean-purchase)) (cadr (car remaining-data)))) sum-of-square-f)
              sum-of-square-m)]
            [else
             (calculate-sd
              (cdr remaining-data)
              sum-of-square-f
              (+ (square (- (cadr (cadr f-and-m-mean-purchase)) (cadr (car remaining-data)))) sum-of-square-m))]))))

; List with marital status and amount purchased of each case in blackfridayfile
(define marital-status-purchase
  (select-columns final-data (list 6 7)))

; List with means of amount purchased of marital status
(define marital-status-mean-purchase
  (let calculate-mean ([remaining-data marital-status-purchase] 
                       [sum-married 0]
                       [sum-unmarried 0])
    (let* ([total-married-and-unmarried (map cadr (tally-all (map (section list-ref <> 6) final-data)))])
      (cond [(null? remaining-data)
             (map cons (list "Married" "Unmarried") (map list (list (round (/ sum-married (- (car total-married-and-unmarried) 1)))
                                                                    (round (/ sum-unmarried (- (cadr total-married-and-unmarried) 1))))))]
            [(equal? 1 (car (car remaining-data)))
             (calculate-mean (cdr remaining-data) (+ (cadr (car remaining-data)) sum-married) sum-unmarried)]
            [else
             (calculate-mean (cdr remaining-data) sum-married (+ (cadr (car remaining-data)) sum-unmarried))]))))

;List with standard deviations of amount purchased of marital status
(define marital-status-sd
  (let calculate-sd ([remaining-data marital-status-purchase]
                     [sum-of-square-married 0]
                     [sum-of-square-unmarried 0])
    (let* ([total-married-and-unmarried (map cadr (tally-all (map (section list-ref <> 6) final-data)))])
      (cond [(null? remaining-data)
             (map cons (list "Married" "Unmarried") (map list (list (round (sqrt (/ sum-of-square-married (car total-married-and-unmarried))))
                                                                    (round (sqrt (/ sum-of-square-unmarried (cadr total-married-and-unmarried)))))))]
            [(equal? 1 (car (car remaining-data)))
             (calculate-sd
              (cdr remaining-data)
              (+ (square (- (cadr (cadr marital-status-mean-purchase)) (cadr (car remaining-data)))) sum-of-square-married)
              sum-of-square-unmarried)]
            [else
             (calculate-sd
              (cdr remaining-data)
              sum-of-square-married
              (+ (square (- (cadr (cadr marital-status-mean-purchase)) (cadr (car remaining-data)))) sum-of-square-unmarried))]))))


;----------------------------------------------------
; 4. LINEAR REGRESSION ATTEMPT
 
(define add-age
  (lambda (lst str)
    (if (equal? (list-ref (reverse lst) 4) str)
        (cons 1 lst)
        (cons 0 lst))))


(define add-all-age
  (lambda (lst)
    (add-age (add-age (add-age (add-age (add-age (add-age lst "0-17") "18-25") "26-35") "36-45") "46-50") "51-55")))


(define add-sex
  (lambda (lst)
    (if (equal? (list-ref (reverse lst) 5) "F") 
        (cons 1 lst)
        (cons 0 lst)))) 


(define add-marital-status
  (lambda (lst)
    (if (equal? (cadr (reverse lst)) 1)
        (cons 1 lst)
        (cons 0 lst))))


(define category-data
  (map cons (make-list (length (map add-marital-status (map add-sex (map add-all-age final-data)))) 1)
       (select-columns (map add-marital-status (map add-sex (map add-all-age final-data))) (list 0 1 2 3 4 5 6 7 15))))


(define estimate (lambda (lst coeff-lst)
                   (square (-  (car (reverse lst)) (+ (car coeff-lst) (* (cadr coeff-lst) (list-ref lst 1)))))))

(define sum-of-squares
  (lambda (data final-coeff-lst)
    (reduce + (map (section estimate <> final-coeff-lst) data))))

(define regression-1
  (lambda (coeff-lst data)
    (let marital-coeff ([current-coeff (cadr coeff-lst)]
                        [n 1000])
      (let* ([new-coeff (list->vector coeff-lst)]
             [old-coeff coeff-lst])
        (cond [(equal? n 0)
               current-coeff]
              [(<= (sum-of-squares data (vector->list new-coeff)) (sum-of-squares data old-coeff))
               (vector-set! new-coeff 1 (+ (vector-ref new-coeff 1) 0.5))
               (marital-coeff (cadr coeff-lst)
                              (- n 1))]
              [(> (sum-of-squares data (vector->list new-coeff)) (sum-of-squares data old-coeff))
               (vector-set! new-coeff 1 (- (vector-ref new-coeff 1) 0.5))
               (marital-coeff (cadr coeff-lst)
                              (- n 1))])))))

(define regression-2
  (lambda (coeff-lst data)
    (let marital-coeff ([current-coeff (cadr coeff-lst)]
                        [coeffs (list->vector coeff-lst)]
                        [n 1000])
      (let ([coeffs-vec-add (vector-set! coeffs 1 (+ (vector-ref (list->vector coeff-lst) 1) 0.5))]
            [coeffs-vec-sub (vector-set! coeffs 1 (- (vector-ref (list->vector coeff-lst) 1) 0.5))])
        (cond [(equal? n 0)
               current-coeff]
              [(<= (sum-of-squares data (vector->list coeffs-vec-add)) (sum-of-squares data (vector->list coeffs)))                                                                                                                                                                                                                                           
               (marital-coeff (cadr coeff-lst)
                              (vector->list coeffs-vec-add)
                              (- n 1))]
              [(> (sum-of-squares data (vector->list coeffs-vec-sub)) (sum-of-squares data (vector->list coeffs)))
               (marital-coeff (cadr coeff-lst)
                              (vector->list (coeffs-vec-sub)
                                            (- n 1)))])))))

