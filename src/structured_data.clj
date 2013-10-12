(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x0 x1 x2]]
  (+ x0 x2))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [[[outer-x1 outer-y1] [outer-x2 outer-y2]] [[x1 y1] [x2 y2]]]
  (and (<= outer-x1 x1 outer-x2) (<= outer-x1 x2 outer-x2) (<= outer-y1 y1 outer-y2) (<= outer-y1 y2 outer-y2)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new-authors (conj (get book :authors) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (= nil (get author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (second x))]
    (map second-element collection)))

(defn titles [books]
  (let [title (fn [book] (get book :title))]
    (map title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (def new-authors (set (get book :authors)))
  (assoc book :authors new-authors))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [get-authors (fn [book] (get book :authors))]
  (apply clojure.set/union (map get-authors books) )))

(defn all-author-names [books]
  (let [author-name (fn [author] (get author :name))]
    (set (concat (map author-name (authors books))))))

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
