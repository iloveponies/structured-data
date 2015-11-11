(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y] [(get v 0) (get v 2)]]
        (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  ))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and (= (- x2 x1) (- y2 y1)))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]

    (if (and (<= x1 px x2) (<= y1 py y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]

    (and (<= ox1 ix1 ix2 ox2) (<= oy1 iy1 iy2 oy2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)
        new-book (assoc book :authors new-authors)]
    new-book))

(defn alive? [author]
  (nil? (:death-year author))
  )

(defn element-lengths [col]
  (map (fn [x] (count x)) col))

(defn second-elements [col]
  (let [sec (fn [v] (get v 1))]
    (map sec col)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [sequence]
  (let [vector (set sequence)]
    (not= (count vector) (count sequence))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [book-authors (:authors book)]
    (contains? book-authors author)))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (apply clojure.set/union (map (fn [b] (map :name (:authors b))) books))))

(defn author->string [author]
  (let [name (:name author)]
    (cond
      (contains? author :death-year) (str name " (" (:birth-year author) " - " (:death-year author) ")")
      (contains? author :birth-year) (str name " (" (:birth-year author) " - )")
      :else name
      )
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
    (= (count books) 0)   "No books."
    (= (count books) 1)   (str "1 book. " (apply book->string books) ".")
    :else                 (str (apply str (count books) " books. " (interpose ". " (map book->string books))) ".")
    )
  )

(defn books-by-author [author books]
  (let [is-author (fn [book] (has-author? book author))]
    (filter is-author books))
  )

(defn author-by-name [name authors]
  (let [is-author (fn [author] (= name (:name author)))]
    (first (filter is-author authors)))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (let [authors (living-authors (:authors book))]
    (not (empty? authors))
    )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
