(ns structured-data)

; Using let to give name for local value and call to Java Math.pow -method.
(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
  )

; Takes a vector and returns the sum of it's first and third element.
(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

; Adds string at the end of the vector.
(defn cutify [v]
  (conj v "<3"))

; Destructuring vector.
(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x1 x2) (- y1 y2))
    )
  )

(defn area [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))
    )
  )

; Check if point is inside the rectangle
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
    )
  )

; Check if inner rectangle is inside outer rectangle
(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] inner]
    (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4]))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (count (:authors book)))
  )

; Add new-author as a author of the book
(defn add-author [book new-author]
  (let [book-authors (:authors book)]
  (assoc book :authors (conj book-authors new-author))
  ))

;
(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [secondz (fn [col] (get col 1))]
    (seq (map secondz collection)))
  )

(defn titles [books]
  (let [titlez (fn [book] (:title book))]
    (seq (map titlez books))
    )
  )

; Check if sequence is monotonic -> either increasing or decreasing
(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

; Retunr n start-asterisks inside a string
(defn stars [n]
  (apply str (repeat n "*"))
  )

; Add element to set if it is not there yet, remove otherwise
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq)) true false)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

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
