(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    )
  )

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ b a))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ c] v]
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
  (if (== (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (if (and (contains-point? outer (point x3 y3)) (contains-point? outer (point x4 y4)))
      true
      false)))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))


(defn all-author-names [books]
  (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author) (if (contains? author :birth-year)
                        (str " (" (:birth-year author) " - " (:death-year author) ")")))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " [(:title book) (authors->string (:authors book))])))

(defn books->string [books]
  (let [book-number (cond (= (count books) 1) (str (count books) " book. ")
                          (< (count books) 1) (str "No books")
                          (> (count books) 1) (str (count books) " books. "))]
    (str (apply str book-number (interpose ". " (map book->string books))) ".")
       )
  )


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author))books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author)))authors)))


(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
