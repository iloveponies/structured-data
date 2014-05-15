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


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  :-)

(defn height [rectangle]
  :-)

(defn square? [rectangle]
  :-)

(defn area [rectangle]
  :-)

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
