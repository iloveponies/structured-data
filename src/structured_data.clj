(ns structured-data)

(defn do-a-thing [x]
  (let [summa (+ x x)]
    (Math/pow summa summa))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (if (vector? v)
    (conj v "<3")
    v
    )
  )

(defn spiff-destructuring [v]
  (let [[x _ y ] v]
    (+ x y)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _] ] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[_ y1] [_ y2] ] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2] ] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2] ] outer
        [[ix1 iy1] [ix2 iy2] ] inner ]
    (and (<= ox1 ix1 ox2) (<= oy1 iy1 oy2)
         (<= ox1 ix2 ox2) (<= oy1 iy2 oy2)))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
  (let [authors     (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

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
