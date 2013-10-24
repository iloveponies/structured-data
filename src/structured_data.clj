(ns structured-data)

(defn do-a-thing [x]
  (let [zop (+ x x)]
    (Math/pow zop zop)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2)))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2)))
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))
  )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false)
  )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author)))
  )

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true)
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [getSecond (fn [x] (get x 1))]
    (map getSecond collection))
  )

(defn titles [books]
  (map :title books)
  )

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

