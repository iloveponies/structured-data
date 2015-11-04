(ns structured-data)

(defn do-a-thing [x]
  (let [xs (+ x x)]
    (Math/pow xs xs)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x, y, z]]
  ;(let [[x, y, z] v]
    (+ x z)
    );)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
       (and
        (and (<= px x2) (>= px x1))
        (and (<= py y2) (>= py y1))
      )
    ))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1][ox2 oy2]] outer
        [[ix1 iy1][ix2 iy2]] inner ]
      (and
       (and (<= ix2 ox2) (>= ix1 ox1))
       (and (<= iy2 oy2) (>= iy1 oy1))
       )
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [my-length (fn [foo] (count foo))]
    (map my-length collection)))

(defn second-elements [collection]
  (let [second-e (fn [my-list] (second my-list))]
    (map second-e collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (< (first a-seq) (second a-seq))
    (apply <= a-seq) (apply >= a-seq))
    )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [ bar (fn [author] (:name author))
        foo (fn [book] (map bar (:authors book)))]
  (clojure.set/union :name (map foo books))
  ))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
    (set (apply concat (map author-names books))))

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
