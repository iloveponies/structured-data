(ns structured-data)

(defn do-a-thing [x]
  (let [v (+ x x)]
    (Math/pow v v)
  ))

(defn spiff [v]
  (let [e1 (get v 0)
        e2 (get v 2)]
    (+ e1 e2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[e1 _ e2] v]
    (+ e1 e2)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [cur_authors (:authors book)]
    (let [new_authors (conj (:authors book) new-author)]
      (assoc book :authors new_authors)
      )
    )
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (seq (map count collection)))

(defn second-elements [collection]
  (seq (map (fn[x] (get x 1)) collection)))

(defn titles [books]
  (seq (map :title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set old-authors))
    ))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
    (if (contains? author :death-year)
      (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
      (str (:name author) " (" (:birth-year author) " - )"))
    (str (:name author))
  ))

(defn authors->string [authors]
  (if (empty? authors)
    (str "")
    (if (= (count authors) 1)
      (author->string (first authors))
      (apply str (interpose ", " (map author->string authors))))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (zero? (count books))
    (str "No books.")
    (if (== 1 (count books))
      (str "1 book. " (book->string (first books)) ".")
      (str (count books) " books. "
           (apply str (interpose ", " (map book->string books))) ".")
    )))

(defn books-by-author [author books]
  (filter (fn[x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn[x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true
  ))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

