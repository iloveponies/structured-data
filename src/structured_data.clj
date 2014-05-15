(ns structured-data)

;; Exercise 1
;; The following function does a thing:
;; (defn do-a-thing [x]
;;   (Math/pow (+ x x) (+ x x)))
;; Change the function do-a-thing so that it uses let to give a name to the common expression (+ x x) in its body.
;; (defn do-a-thing [x]
;;   (Math/pow (+ x x) (+ x x)))
(defn do-a-thing [x]
  (let [x-times-2 (+ x x)]
    (Math/pow x-times-2 x-times-2)))


;; Exercise 2
;; Write the function (spiff v) that takes a vector and returns the sum of the first and third elements of the vector. What happens when you pass in a vector that is too short?
;; (spiff [1 2 3])       ;=> 4
;; (spiff [1 2 3 4 5 6]) ;=> 4
;; (spiff [1 2])         ;=> ?
;; (spiff [])            ;=> ?
(defn spiff [v]
  (+ (get v 0) (get v 2)))

;; Exercise 3
;; Write the function (cutify v) that takes a vector as a parameter and adds "<3" to its end.
;; (cutify []) => ["<3"]
;; (cutify [1 2 3]) => [1 2 3 "<3"]
;; (cutify ["a" "b"]) => ["a" "b" "<3"]
;;
;; vector -> vector
;; Add <3 at the end
(defn cutify [v]
  (conj v "<3"))

;; Exercise 4
;; Rewrite our earlier function spiff by destructuring its parameter. Call this new function spiff-destructuring.
(defn spiff-destructuring [v]
  (let [[a _ c] v]
    (+ a c)))
;; Simpler
(defn spiff-destructuring [[a _ c]]
  (+ a c))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;; Exercise 5
;; Write the functions (height rectangle) and (width rectangle) that return the height and width of the given rectangle. Use destructuring.
(defn width [[[x1 y1] [x2 y2]]]
  (let [res (- x1 x2)]
    (if (>= res 0 )
      res
      (- res))))

(defn height [[[x1 y1] [x2 y2]]]
  (let [res (- y1 y2)]
    (if (>= res 0 )
      res
      (- res))))

;; (height (rectangle [1 1] [5 1])) ;=> 0
;; (height (rectangle [1 1] [5 5])) ;=> 4
;; (height (rectangle [0 0] [2 3])) ;=> 3

;; (width (rectangle [1 1] [5 1]))  ;=> 4
;; (width (rectangle [1 1] [1 1]))  ;=> 0
;; (width (rectangle [3 1] [10 4])) ;=> 7


;; Exercise 6
;; Write the function (square? rectangle) that returns true if rectangle is a square and otherwise false.
;; (square? (rectangle [1 1] [2 2])) ;=> true
;; (square? (rectangle [1 1] [2 3])) ;=> false
;; (square? (rectangle [1 1] [1 1])) ;=> true
;; (square? (rectangle [3 2] [1 0])) ;=> true
;; (square? (rectangle [3 2] [1 1])) ;=> false
(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

;; Exercise 7
;; Write the function (area rectangle) that returns the area of the given rectangle.
(defn area [rectangle]
  (* (height rectangle) (width rectangle)))
;; (area (rectangle [1 1] [5 1]))  ;=> 0
;; (area (rectangle [0 0] [1 1]))  ;=> 1
;; (area (rectangle [0 0] [4 3]))  ;=> 12
;; (area (rectangle [3 1] [10 4])) ;=> 21


;; Exercise 8
;; Write the function (contains-point? rectangle point) that returns true if rectangle contains point and otherwise false.
;; Remember that you can give <= multiple parameters. (<= x y z) returns true if x≤y≤z holds. Otherwise false.
;; Hint: and is useful.
;; use destructuring.
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    ;; Generalized version that can handle x2 < x1, etc
    (if (and (or (<= x1 x x2) (>= x1 x x2))
             (or (<= y1 y y2) (>= y1 y y2)))
      true
      false)))
;;
;; no need for if when using predicates
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    ;; Generalized version that can handle x2 < x1, etc
    (and (or (<= x1 x x2) (>= x1 x x2))
         (or (<= y1 y y2) (>= y1 y y2)))))
;;
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point 2 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))           ;=> false
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))            ;=> false
(contains-point? (rectangle [1 1] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [1 1] [1 1])
                 (point 1 1))            ;=> true


;; Exercise 9
;; Write the function (contains-rectangle? outer inner) that returns true if the rectangle inner is inside the rectangle outer and otherwise false.
;; Hint: use contains-point?
;;
;; Two nested vectors -> Bool
;; Check if all four points are within the first rectangle
;; Get all four points from inner
(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  ;; Check all four points with AND condition
  (and (contains-point? outer [x1 y1])
       (contains-point? outer [x1 y2])
       (contains-point? outer [x2 y1])
       (contains-point? outer [x2 y2])))
;;
;; for solution
(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  ;; Check all four points with for
  (let [bool-4-points (for [x [x1 x2] y [y1 y2]]
                        (contains-point? outer [x y]))]
    ;; true if true X 4
    (every? identity bool-4-points)))
;;
(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false


;; Exercise 10
;; Write the function (title-length book) that counts the length of the book’s title.
;; (title-length cities)         ;=> 21
;; (title-length wild-seed)      ;=> 9
;; (title-length little-schemer) ;=> 18
;; Map -> Number
;; Length of :title element of the map
(defn title-length [book]
  (count (:title book)))

;; Exercise 11
;; Write the function (author-count book) that returns the amount of authors that book has.
;; (author-count cities)         ;=> 1
;; (author-count wild-seed)      ;=> 1
;; (author-count little-schemer) ;=> 2
(defn author-count [book]
  (count (:authors book)))

;; Exercise 12
;; Write the function (multiple-authors? book) that returns true if book has multiple authors, otherwise false.
;; (multiple-authors? cities)         ;=> false
;; (multiple-authors? wild-seed)      ;=> false
;; (multiple-authors? little-schemer) ;=> true
(defn multiple-authors? [book]
  (>= (author-count book) 2))


;; Exercise 13
;; Use assoc and conj to write the function (add-author book new-author) that takes a book and an author as a parameter and adds author to books authors.
;; Hint: use let to avoid pain
;;
;; map -> map
;; Add author by extracting author part, add author, and conj
(defn add-author [book new-author]
  ;; Extract authors part
  (let [authors (:authors book)]
    ;; Add a new authors to authors part, then add to map
    (assoc book :authors (conj authors new-author))))
(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}


;; 
(defn alive? [author]
  :-)


(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
