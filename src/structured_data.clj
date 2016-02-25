(ns structured-data)

(defn do-a-thing [x]
  (let [dub-x (+ x x)]
    (Math/pow dub-x dub-x)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ c]]
  (+ a c))

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
  (assoc book :authors (conj (:authors book) new-author)))

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
  (let [ foo (fn [book] (set (:authors book)))]
  (apply clojure.set/union (map foo books))
    ))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (set (apply concat (map author-names books))))

(defn author->string [author]
  (let [ n (:name author)
         b (:birth-year author)
         d (:death-year author)
        ]
    (str
        n
        (if b (str " (" b " - ") "")
        (if d d "")
        (if b (str ")") "")
        )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (= (count books) 0) (str "No books.")
    (str (count books) (if (= (count books) 1)" book. " " books. ") (apply str (interpose ", " (map book->string books))) ".")
    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (:name author) name) author nil)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
