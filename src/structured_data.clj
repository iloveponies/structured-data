(ns structured-data)

; exercise-1
(defn do-a-thing [x]
  (let [y (+ x x)]
  (Math/pow y y)))

; exercise-2
(defn spiff [v]
  (+ (get v 0) (get v 2)))

; exercise-3
(defn cutify [v]
  (conj v "<3"))

; exercise-4
(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

; exercise-5
(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- x1 x0)))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- y1 y0)))

; exercise-6
(defn square? [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (== (- x0 x1) (- y0 y1))))

; exercise-7
(defn area [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
      (* (- x0 x1) (- y0 y1))))

; exercise-8
(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [x y] point]
    (and (<= x0 x x1) 
         (<= y0 y y1))))

; exercise-9
(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
       (and (contains-point? outer bottom-left) 
            (contains-point? outer top-right))))

; exercise-10
(defn title-length [book]
  (count (:title book)))

; exercise-11
(defn author-count [book]
  (count (:authors book)))

; exercise-12
(defn multiple-authors? [book]
  (> (author-count book) 1))

; exercise-13
(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

; exercise-14
(defn alive? [author]
  (not (contains? author :death-year)))

; exercise-15
(defn element-lengths [collection]
  (map count collection))

; exercise-16
(defn second-elements [collection]
  (let [get-second-element (fn [xs] (get xs 1))]
    (map get-second-element collection)))

; exercise-17
(defn titles [books]
  (map :title books))

; exercise-18
; apply works like this:
; (apply function [arg1 arg2 arg3 ...]) => (function arg1 arg2 arg3 ...)
(defn stars [n]
  (apply str (repeat n "*")))

; exercise-19
(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

; exercise-20
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

; exercise-21
(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

; exercise-22
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

; exercise-23
(defn has-author? [book author]
  (contains? (:authors book) author))

; exercise-24
(defn authors [books]
  (apply clojure.set/union (map :authors books)))

; exercise-25
(defn all-author-names [books]
  (set (map :name (authors books))))

; exercise-26
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
