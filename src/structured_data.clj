(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
)

(defn spiff [v]
  (cond
    (empty? v) 0
    (> 3 (count v)) (get v 0)
    :else (+ (get v 0) (get v 2))
  )
)


(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[a b c d] v]
    (cond
      (= a nil) 0
      (= c nil) a
      :else (+ a c)
    )
  )
)


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> x1 x2) (- x1 x2) (- x2 x1)))
)


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2) (- y1 y2) (- y2 y1)))
)

(defn square? [rectangle]
  (== (width rectangle) (height rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
          [px py] point]
      (and (>= px x1) (<= px x2)
           (>= py y1) (<= py y2))
      )
)

(defn contains-rectangle? [outer inner]
    (let [[[x1 y1] [x2 y2]] outer
          [[px1 py1] [px2 py2]] inner]
      (and (contains-point? outer [px1 py1])
           (contains-point? outer [px2 py2])
      )
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (< 1 (author-count book))
)

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))


(defn alive? [author]
    (not (contains? author :death-year))
)


(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [just-second (fn [x] (get x 1) )]
      (seq (map just-second collection))
  )
)

(defn titles [books]
  (seq (map :title books))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq)))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (apply clojure.set/union (map :authors books))))
)

(defn author->string [author]
  (let [year->string (fn [a] (if (contains? a :birth-year)
                              (str " (" (:birth-year a) " - " (:death-year a) ")" )))]
    (str (:name author) (year->string author))
  )
)

(defn authors->string [authors]
  (let [add-authors (fn [as] (str ", " (authors->string (rest authors))))]
    (apply str (author->string (first authors))
           (if (< 1 (count authors)) (add-authors authors)))
  )
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (let [add-comma (fn [s] (str s ", "))]
    (cond
      (= 0 (count books)) "No books."
      (< 1 (count books)) (str (count books) " books. " (apply str (map add-comma (map book->string books))) ".")
      :else (str "1 book. " (apply str (map book->string books)) ".")
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
 (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
