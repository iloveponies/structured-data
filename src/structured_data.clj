(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
  )

(defn spiff [v]
  (if (> (count v) 2)
    (+ (first v) (get v 2))
    "tow few element in vector"
    )
  )

(defn cutify [v]
  (if (vector? v)
    (conj v "<3")
    "not a vector"
    ) 
  )

(defn spiff-destructuring [v]
  (if (and (vector? v) (> (count v) 2))
    (let [[a foo b] v]
      (+ a b))
    "not a vector or two few elements"
    )
  )

(defn sum-pairs [first-pair second-pair]
  [(+ (first  first-pair) (first  second-pair))
   (+ (second first-pair) (second second-pair))])
(defn sum-pairs2 [[x1 y1] [x2 y2]]
    [(+ x1 x2) (+ y1 y2)])
(sum-pairs [42 5]   [-42 -5])   ;=> [0 0]
(sum-pairs [64 256] [-51 -219]) ;=> [13 37]

; ====================================================================
; let s doing some geometry
; ====================================================================

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point] "(contains-point? (rectangle [0 0] [2 2]) (point 1 1))"
  (let [[[x1 y1] [x2 y2]] rectangle [z1 z2] point]
   (and (<= x1 z1 x2) (<= y1 z2 y2))
    )
  )

(defn contains-rectangle? [outer inner] "(contains-rectangle? (rectangle [0 0] [3 3]) (rectangle [1 1] [2 2]))"
  (let [[[x1 y1] [x2 y2]] outer 
        [[a1 b1] [a2 b2]] inner]
    (and (contains-point? (rectangle [x1 y1] [x2 y2]) (point a1 b1))
         (contains-point? (rectangle [x1 y1] [x2 y2]) (point a2 b2))
         )
    )
  )

; ====================================================================
; After geometry, books
; ====================================================================

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler" :birth-year 1947 :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;(
;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed", :authors [octavia]})
;(def embassytown {:title "Embassytown", :authors [china]})
;(def little-schemer {:title "The Little Schemer" :authors [friedman, felleisen]})
(def books [cities, wild-seed, embassytown, little-schemer])

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author] "(add-author little-schemer {:name \"Gerald J. Sussman\"})"
  (assoc book :authors (conj (book :authors) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondelem (fn [coll] (get coll 1))]
    (map secondelem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  (apply str (repeat n "*")))

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
