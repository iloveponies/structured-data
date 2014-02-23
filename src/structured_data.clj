(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] 
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle)(width rectangle)))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (>= px x1) (<= px x2) (>= py y1) (<= py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (>= ix1 ox1) (>= iy1 oy1) (<= ix2 ox2) (<= iy2 oy2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)
    ))

(defn alive? [author]
  (not (contains? author :death-year)
    ))

(defn element-lengths [collection]
  (map (fn [item] (count item)) collection))

(defn second-elements [collection]
  (let [get-second-element (fn [collection] (get collection 1))]
  (seq (map get-second-element collection))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
    (or (apply <= a-seq)
        (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (= (count (:authors book)) (count (conj (:authors book) author))))

(defn authors [books]
  (apply clojure.set/union (apply :authors books)))

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
