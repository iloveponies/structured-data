(ns structured-data
  (:use clojure.repl))

(defn do-a-thing [x]
  (let [x2 (+ x x)]
  (Math/pow x2 x2)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(conj [1 2 3] 4 5)
(conj '(1 2 3) 4 5)
(conj #{1 2 3} 4 5)
(conj {2 3} {0 1} {1 2})
(conj #{2 3} 0 1)

(cons 3 '(1 2 3))
(cons 3 #{1 2 3})
(cons 4 [1 2 3])
(cons {2 3} {0 1 1 2})

(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  :-)

(defn contains-rectangle? [outer inner]
  :-)

(defn title-length [book]
  :-)

(defn author-count [book]
  :-)

(defn multiple-authors? [book]
  :-)

(defn add-author [book new-author]
  :-)

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
