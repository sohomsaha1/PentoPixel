;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; Sohom Saha
;; Gesture Recognizer 



;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       






;; (get-x lst): This function consumes a point(lst) and returns its x-coordinate
;; Examples:
(check-expect (get-x(list 2 3)) 2)

;; get-x: Point -> Num
(define (get-x lst)(first lst))


;; (get-y lst): This function consumes a point(lst) and returns its y-coordinate
;; Examples:
(check-expect (get-y(list 2 3)) 3)

;; get-y: Point -> Num
(define (get-y lst)(first(rest lst)))


;; (translate-gesture list1 x y): This function consumes a gesture (list1) and 2 values (x,y)
;;                                and translates each point in lst1 by its x or y value.
;; Examples:
(check-expect (translate-gesture (list(list 1 0)(list 2 3)) 1 2)(list(list 2 2)(list 3 5)))
(check-expect (translate-gesture (list(list 0 0)) 5 5)(list(list 5 5)))

;; translate-gesture: Gesture -> Gesture
(define (translate-gesture list1 x y)
   (cond[(empty? list1) empty]
       [ else (cons (list (+ x (first(first list1)))
                          (+ y (first(rest(first list1)))))
                    (translate-gesture (rest list1) x y))]))


;; (scale-gesture list1 x y): This function consumes a gesture (list1) and 2 values (x,y)
;;                                and scales each x or y point in list1 by its x or y value.
;; Examples:
(check-expect (scale-gesture (list(list 1 1)(list 2 3)) 1 2)(list(list 1 2)(list 2 6)))
(check-expect (scale-gesture (list(list 0 1)) 5 5)(list(list 0 5)))

;; scale-gesture: Gesture -> Gesture
(define (scale-gesture list1 x y)
  (cond[(empty? list1) empty]
       [ else (cons (list (* x (first(first list1)))
                          (* y (first(rest(first list1)))))
                    (scale-gesture (rest list1) x y))]))
 

;; (get-xs list1): This function consumes a gesture(list1)
;;                 and returns all the x-coordinates in every point
;; Examples
(check-expect (get-xs(list(list 1 2)(list 2 3)))(list 1 2))

;; get-xs: Gesture -> (listof Num)
(define (get-xs list1)
  (cond[(empty? list1) empty]
       [ else (cons (get-x (first list1))(get-xs(rest list1)))]))


;; (get-ys list1): This function consumes a gesture(list1)
;;                 and returns all the y-coordinates in every point
;; Examples
(check-expect (get-ys(list(list 1 2)(list 2 3)))(list 2 3))

;; get-ys: Gesture -> (listof Num)
(define (get-ys list1)
  (cond[(empty? list1) empty]
       [ else (cons (get-y (first list1))(get-ys(rest list1)))]))


;; (min1 listx num): This function consumes listx and
;;                   returns the smallest value from that list
;; Examples:
(check-expect (min1 (list 1 2 3) 100000000) 1)

;; min1: (listof Nuum) Num -> Num
(define (min1 listx num)
  (cond[(empty? listx) num]
       [ (> num  (first listx))(min1 (rest listx) (first listx))]
       [ else (min1 (rest listx) num)]))


;; (max1 listx num): This function consumes listx and
;;                   returns the largest value from that list
;; Examples:
(check-expect (max1 (list 1 2 3) -100000000) 3)

;; max1: (listof Num) Num -> Num
(define (max1 listx num)
  (cond[(empty? listx) num]
       [ (< num  (first listx))(max1 (rest listx) (first listx))]
       [ else (max1 (rest listx) num)]))


;; (get-b-box list1): This function consumes a gesture(list1)
;;                    and returns its bounding-box as defined.
;; Examples:
(check-expect (get-b-box(list(list 100 0)(list 200 100)
                             (list 100 200)(list 0 100)(list 100 0)))
                                           (list(list 0 0)(list 200 200)))

;; get-b-box: Gesture -> Gesture
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point
(define (get-b-box list1)
                   (list (list (min1(get-xs list1)100000000)
                               (min1(get-ys list1)100000000))
                               (list (max1(get-xs list1)-100000000)
                                     (max1(get-ys list1)-100000000))))
 



;; (gesture-length listX): This function consumes a gesture(listX)
;;                         with 2 points and returns the distance between them.
;; Examples:
(check-within (gesture-length (list(list 1 2)(list 2 3))) 1.41 0.01)
(check-within (gesture-length (list(list 4 5)(list 0 0))) 6.40 0.01)

;; gesture-length: Gesture -> Num
;; Requires: Gesture is non-empty
(define (gesture-length listX)
  (cond[(empty? listX) 0]
       [ (=(length listX)1) 0]
       [ else (sqrt(+(expt(- (get-x (second listX))
                            (get-x (first listX)))2)
                     (expt(- (get-y (second listX))
                            (get-y(first listX)))2)))]))

;; Tests:
(check-within (gesture-length (list(list 0 0)(list 0 0))) 0 0.01)
(check-within (gesture-length (list(list 4 5)(list 0 0))) 6.40 0.01)
(check-within (gesture-length (list(list 4 5)(list 4 5))) 0 0.01)
(check-within (gesture-length (list(list 12 12.1)(list 14.4 100.1))) 88.03 0.01)



;; (get-points-help  listP listN counter): This function consumes a gesture(listP)
;; and a non-decreasing list of Nat(lisN) in the range of [0...(n-1] where n is the
;; number of points in listN. This function produces a gesture where each point in the
;; produced gesture is indexed by one element of the list of Nat consumed. 
;;
;; Examples:
(check-expect (get-points-help (list(list 1 2)(list 3 4)(list 5 6))(list 0 0 1) 0)
                                                  (list(list 1 2)(list 1 2)(list 3 4)))
(check-expect (get-points-help (list(list 0 0)(list 1 1)(list  2 2))(list 0 1 2) 0)
                                                  (list(list 0 0)(list 1 1)(list 2 2)))

;; get-points-help: Gesture (listof Nat) Num -> Gesture
(define (get-points-help listP listN counter)
  (cond[(empty? listN) empty]
       [(empty? listP) empty]
       [(= counter (first listN))(cons (first listP)
                                       (get-points-help listP (rest listN)counter))]
       [ else (get-points-help (rest listP) listN (+ 1 counter))]))



;; (get-points listP listN): This function consumes a gesture(listP)
;; and a non-decreasing list of Nat(lisN) in the range of [0...(n-1] where n is the
;; number of points in listN. This function produces a gesture where each point in the
;; produced gesture is inddexed by one element of the list of Nat consumed. 
;;
;; Examples:
(check-expect (get-points (list(list 1 2)(list 3 4)(list 5 6))(list 0 0 1))
                                                  (list(list 1 2)(list 1 2)(list 3 4)))
(check-expect (get-points (list(list 0 0)(list 1 1)(list  2 2))(list 0 1 2))
                                                  (list(list 0 0)(list 1 1)(list 2 2)))

;; get-points: Gesture (listof Nat) -> Gesture
;; Requires: Gesture is non-empty
(define (get-points listP listN)(get-points-help listP listN 0))

;; Tests:
(check-expect (get-points (list(list 1 2))(list 0 0 0))
                                                  (list(list 1 2)(list 1 2)(list 1 2)))
(check-expect (get-points empty empty)empty)

(check-expect (get-points (list(list 12 1)(list 23 2)(list  11 11)(list 12 12))(list 1 2 3))
                                                  (list(list 23 2)(list 11 11)(list 12 12)))






;; (lastL listG): This function consumes listG and returns a list containing its last value.
;; Examples:
(check-expect (lastL(list 1 2 3)) (list 3))
(check-expect (lastL(list "hello"))(list "hello"))

;; lastL: (listof Any) -> (listof Any)
(define (lastL listG)
  (cond [ (empty? listG) empty]
        [ (= 1 (length listG))(cons (first listG)(lastL (rest listG)))]
        [ else (lastL (rest listG))]))



;; five-sample-help: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample-help listN  counter)
  (cond[(empty? listN) empty]
       [ ( = 5 counter) empty]
       [ (= 0 counter)(cons(first listN)(five-sample-help listN (+ 1 counter)))]
       [ (= 1 counter)(cons (first
                             (get-points listN (list (floor (* 0.25 (length listN))))))
                                               (five-sample-help listN (+ 1 counter)))]
       [ (= 2 counter)(cons (first
                             (get-points listN (list (floor (* 0.5 (length listN))))))
                                               (five-sample-help listN (+ 1 counter)))]
       [ (= 3 counter)(cons
                       (first (get-points listN (list (floor (* 0.75 (length listN))))))
                                                (five-sample-help listN (+ 1 counter)))]
       [ (= 4 counter) (cons (first(lastL listN)) (five-sample-help listN (+ 1 counter)))]))


;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 2 1) (list 2 2) (list 2 4) (list 5 2)))
              (list (list 2 1) (list 2 2) (list 2 4) (list 5 2) (list 5 2)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))


;; five-sample: Gesture -> Gesture
;; requires: Gesture is non-empty
(define (five-sample listN)(five-sample-help listN 0))



;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))



;; (min-x list1 num): This function takes in a gesture(list1)
;;  and finds the smallest x-coordinate value within all the Points in list1.
;; Examples:
(check-expect (min-x (list(list 5 2)(list 2 12)) 1000)2)
(check-expect (min-x(list(list 0 4)) 1000) 0)

;; min-x: Gesture Num -> Num
;; Requires: Gesture is not empty
(define (min-x list1 num)
  (cond[(empty? list1) num]
       [(< num (first(first list1)))(min-x (rest list1) num)] 
       [else (min-x(rest list1)(first(first list1)))]))


;; (min-y list1 num): This function takes in a gesture(list1)
;;  and finds the smallest y-coordinate value within all the Points in list1.
;; Examples:
(check-expect (min-y (list(list 5 2)(list 2 12)(list 45 100)) 1000)2)
(check-expect (min-y(list(list 0 4)) 1000) 4)

;; min-y: Gesture Num -> Num
;; Requires: Gesture is not empty
(define (min-y list1 num)
  (cond[(empty? list1) num]
       [(< num (second(first list1)))(min-y (rest list1) num)]
       [else (min-y(rest list1)(second(first list1)))]))


;; (originer list1 list2): This function takes in a gesture(list1)
;; and moves it to the origin by shifting the minimum x and y values.

;; Examples:
;; Requires: Gesture is non-empty
(check-expect (originer(list (list 5 5) (list 2 2))
                             (list (list 5 5) (list 2 2)))
                                   (list(list 3 3)(list 0 0)))

(check-expect (originer(list (list 5 5) (list 2 2)(list 0 1))
                             (list (list 5 5) (list 2 2)(list 0 1)))
                                   (list(list 5 4)(list 2 1)(list 0 0)))

;; originer: Gesture Gesture -> Gesture
;; Requires: list1 = list2 and that both are non-empty
(define (originer list1 list2)
  (cond [(empty? list1) empty]
        [ else (cons (list
               (-(first(first list1))(min-x list2 1000))
               (-(second(first list1)) (min-y list2 1000)))
                        (originer (rest list1) list2))]))



;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 4 4) (list 2 2)) 3 1)
              (list (list 6 2) (list 0 0)))
(check-expect (move-and-scale (list (list 5 5)) 5 5) (list (list 0 0)))


;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale list1 x y)
  (cond[(empty? list1) empty]
       [ (= 1(length list1))(list(list 0 0))]
       [else (scale-gesture(originer list1 list1) x y)]))



;; Tests:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))



;; 3ciii) constants:
(define min-width 30)
(define min-height 30)
(define norm-size 200)



;; (max-x list1 num): This function consumes a gesture(list1) and returns
;; the largest x-coordinate from any of the Points in list1

;; Examples:
(check-expect (max-x (list(list 2 3)(list 3 4)(list 4 5)) -1000) 4)
(check-expect (max-x (list(list 2 3)) -1000) 2)

;; max-x: Gesture Num -> Num
;; Requires: Gesture is non-empty
(define (max-x list1 num)
  (cond[(empty? list1) num]
       [(> num (first(first list1)))(max-x (rest list1) num)]
       [else (max-x(rest list1)(first(first list1)))]))


;; (max-y list1 num): This function consumes a gesture(list1) and returns
;; the largest y-coordinate from any of the Points in list1

;; Examples:
(check-expect (max-y (list(list 2 3)(list 3 4)(list 4 5)) -1000) 5)
(check-expect (max-y (list(list 2 3)) -1000) 3)

;; max-y: Gesture Num -> Num
;; Requires: Gesture is non-empty
(define (max-y list1 num)
  (cond[(empty? list1) num]
       [(> num (second(first list1)))(max-y (rest list1) num)]
       [else (max-y(rest list1)(second(first list1)))]))


;; (x_diff list1): This function consumes a gesture(list1) and outputs
;; whether it is a vertical line or not.

;; Examples:
;; Requres: Gesture is non-empty
(check-expect (x_diff (list(list 1 2)(list 100 2))) false)
(check-expect (x_diff (list(list 11 0)(list 10 5))) true)

;; x_diff: Gesture -> Bool
;; Requires: Gesture must have > 1 Points.
(define (x_diff list1)
  (cond[(<(-(max-x list1 -1000)(min-x list1 1000))min-width) true]
       [ else false]))


;; (y_diff list1): This function consumes a gesture(list1) and outputs
;; whether it is a horizontal line or not

;; Examples:
(check-expect (y_diff (list(list 1 23)(list 100 2))) true)
(check-expect (y_diff (list(list 10 100)(list 10 5))) false)

;; y_diff: Gesture -> Bool
;; Requires: Gesture must have > 1 Points.       
(define (y_diff list1)
  (cond[(<(-(max-y list1 -1000)(min-y list1 1000))min-height) true]
       [ else false]))



;; (get_scale list1): This function takes in a gesture(list1), and outputs a list of the x-scale
;; and y-scale which would be the values to scale the gesture within the bounding box.
;; Examples
(check-within (get_scale (list(list 50 100)(list 2 100)))(list 4.16 0)0.01)
(check-within (get_scale (list(list 50 100)(list 2 100)(list 45 50)))(list 4.16 4)0.01)

;; get_scale: Gesture -> (list Num Num)
;; Requires: Gesture have atleast 2 points.
(define (get_scale list1)
  (cond[(and(= 0(max-y (originer list1 list1) -1000))
            (= 0(max-x (originer list1 list1) -1000)))(list 0 0)]
       [(= 0(max-y (originer list1 list1) -1000))
            (list (/ 200(max-x(originer list1 list1) -1000)) 0)]
       [(= 0(max-x (originer list1 list1) -1000))
            (list 0 (/ 200(max-y(originer list1 list1) -1000)))]
       [(x_diff list1)(list 1(/ 200(max-y (originer list1 list1) -1000)))]
       [(y_diff list1)(list (/ 200(max-x(originer list1 list1) -1000))1)]
       [else (list (/ 200(max-x (originer list1 list1) -1000))
                          (/ 200(max-y (originer list1 list1) -1000)))]))

 


;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty

(define(normalize-gesture list1)
  (move-and-scale list1 (first(get_scale list1))
                        (second(get_scale list1))))
  
 

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)




;; (eq_help list1 list2): This function finds the distance between
;; the two first Points in the gestures:list1 and list2

;; Examples:
(check-within (eq_help (list(list 2 50))(list(list 40 100))) 62.8 0.01)
(check-within (eq_help (list(list 0 0))(list(list 200 200))) 282.84 0.01)

;; eq_help: Gesture Gesture -> Num
;; Requires: list1 and list2 are non-empty
(define (eq_help list1 list2)(sqrt
                              (+
                               (expt
                                 (-(first(first list1))
                                           (first(first list2)))2)
                               (expt
                                 (-(second(first list1))
                                           (second(first list2)))2))))
  
 
;; (g_help list1 list2 num): This function takes in 2 gestures(list1,list2) and outputs
;; a number(num) equivalent to the summation of the length function between two Points,
;; these points being the same index from both list1 and list2.

;; Examples
(check-within (g_help (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))0)141.42 0.01)
(check-within (g_help(list (list 20 20) (list 40 40) (list 60 60) (list 80 80) (list 90 90))
               (list (list 30 20) (list 30 30) (list 40 40) (list 50 50) (list 50 50))0) 151.42 0.01)


;; g_help: Gesture Gesture Num -> Num
;; Requires: Both gestures are of equal length and non-empty
(define (g_help list1 list2 num)
  (cond[ (and(empty? list1)(empty? list2)) num]
       [ else (g_help (rest list1) (rest list2) (+ num (eq_help list1 list2)))]))



;; (geom_convert list1 list2): This function takes in 2 gestures(list1,list2)
;; and applies the function g_help to them.

;; Examples:
(check-within (geom_convert (list(list 2 3)(list 4 5))(list(list 12 3)(list 4 45))) 22 0.01)
(check-within (geom_convert (list(list 21 3)(list 24 5))(list(list 12.1 3)(list 200 45))) 591 0.01)

;; geom_convert: Gesture Gesture -> Num
;; Requires: Both gestures are non-empty
(define (geom_convert list1 list2)(g_help (normalize-gesture(five-sample list1))
                                  (normalize-gesture(five-sample list2))0))
 


;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points

;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match list1 list2)(/(geom_convert list1 list2)5))



;; Tests:
(check-within (geometric-5match
               (list (list 40 40) (list 50 50) (list 60 60) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               28.28 0.01)

(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 25 25) (list 20 20) (list 45 45) (list 40 40) (list 40 40)))
               116.22 0.01)

(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 70 70) (list 90 90) (list 100 100))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               12.57 0.01)




