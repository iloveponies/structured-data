(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2))
  )

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
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (- xtr xbl))
  )

(defn height [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (- ytr ybl))
  )

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle)) true false)
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[xbl ybl] [xtr ytr]] rectangle]

    (if (and (<= xbl px) (<= ybl py) (>= xtr px) (>= ytr py)) true false))
  )

(defn contains-rectangle? [outer inner]
    (if (and (contains-point? outer (first inner)) (contains-point? outer (second inner))) true false))

(defn title-length [book]
  (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false)
  )

(defn add-author [book new-author]
  (let [authors (get book :authors )]
    (assoc book :authors (conj authors new-author)))
  )

(defn alive? [author]
  (if (contains? author :death-year) false true)
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn get-second-element [collection]
  (get collection 1)
  )

(defn second-elements [collection]
  (map get-second-element collection)
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false)
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )


(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq))
         (count a-seq))
    false true)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors)))
  )

(defn has-author? [book author]
  (if (contains? (get book :authors) author) true false)
  )

(defn authors [books]
  (let [author
         (fn [book] (:authors book))]
    (set (apply concat (map author books)))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn author->string [author]
  (let [a-name (get author :name)
        a-death (get author :death-year)
        a-birth (get author :birth-year)]
    (if (and (nil? a-death) (nil? a-birth))
      (str a-name)
      (str a-name " (" a-birth " - " a-death ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
        all-authors (authors->string (get book :authors))]
    (str title ", written by " all-authors)))

(defn books->string [books]
  (let [counter (count books)
        books-str (apply str (interpose ", " (map book->string books)))]
    (cond
     (> counter 1) (str counter " books. " books-str ".")
     (> counter 0) (str "1 book. " books-str ".")
     :else "No books.")))

(defn books-by-author [author books]
  (let [filter-f (fn [book] (has-author? book author))]
    (filter filter-f books)))

(defn author-by-name [name authors]
  (let [filter-f (fn [author] (= (get author :name ) name))]
    (first (filter filter-f authors))))

(defn living-authors [authors]
  (let [filter-f (fn [author] (alive? author))]
    (filter filter-f authors)))

(defn has-a-living-author? [book]
  (let [auhtors-alive (living-authors (get book :authors))]
    (if (< 0 (count auhtors-alive)) true false)))

(defn books-by-living-authors [books]
  (let [filter-f (fn [book] (has-a-living-author? book))]
    (filter filter-f books)))

; %________%
