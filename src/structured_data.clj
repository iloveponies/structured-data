(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (cond
    (>= (count v) 3)
    (+  (first v) (second (rest v)))
    (>= (count v) 1)
    (first v)
    :else
    0))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (cond
    (>= (count v) 3)
    (let [[x y z] v] (+ x z))
    (>= (count v) 1)
    (let [[x] v] x)
    :else
    0))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (#(if (neg? %) (- %) %) (- (first (first rectangle)) (first (second rectangle)))))

(defn height [rectangle]
  (#(if (neg? %) (- %) %) (- (second (first rectangle)) (second (second rectangle)))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (and (or (<= (first (first rectangle)) (first point) (first (second rectangle)))
           (>= (first (first rectangle)) (first point) (first (second rectangle)))
           )
        (or (<= (second (first rectangle)) (second point) (second (second rectangle)))
           (>= (second (first rectangle)) (second point) (second (second rectangle)))
           )
       ))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner))
       (contains-point? outer (second inner))
       ))

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (= (author-count book) 1)
    false
    true
    ))

(defn add-author [book new-author]
  (let [new-a (conj (:authors book) new-author)]
    (assoc book :authors new-a)
    ))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true
    ))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map (fn [c] (get c 1)) collection)
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
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
  (if (= (count a-seq)
         (count (set a-seq)))
    false
    true)
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
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [n (:name author)
        b (:birth-year author)
        d (:death-year author)]
    (if (and (nil? b) (nil? d))
      (str n)
      (str n " (" b " - " d ")")))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [t (:title book)
        a (authors->string (:authors book))]
      (str t ", written by " a))
  )

(defn books->string [books]
  (let [c (count books)
        s (apply str (interpose ", " (map book->string books)))]
    (cond
      (= c 0)
      "No books."
      (= c 1)
      (str c " book. " s ".")
      (>= c 2)
      (str c " books. " s ".")))
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
  (not (every? false? (map alive? (:authors book))))
  ;(true? (first (set (filter (fn [x] (true? x)) (map alive? (:authors book))))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
