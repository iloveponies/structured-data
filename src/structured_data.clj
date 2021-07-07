(ns structured-data)

(defn do-a-thing [x]
  (let [x ( + x x)]
  (Math/pow x x)))

(defn spiff [v]
  (+(get v 0)(get v 2)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [x (get v 0) y (get v 2)]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (if > 0 (- x1 x2))
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (if > 0 (- y1 y2))
    (- y2 y1))
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (mod (+ x1 y1 x2 y2) 2) 0)
    ))

(defn area [rectangle]
  (*(width rectangle)(height rectangle)))


(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
      (and (>= p1 x1) (<= p1 x2) (>= p2 y1) (<= p2 y2))
  ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[a1 b1] [a2 b2]] inner]
  (and (<= x1 a1) (<= y1 b1) (>= x2 a2) (>= y2 b2))
  ))




(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
   (assoc book :authors
     (conj (:authors book) new-author)))


(defn alive? [author]
  (if (contains? author :death-year)
    false
    true
  ))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [secondIndex (fn [x] (get x 1))]
    (map secondIndex collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))


(defn stars [n]
  (apply str (repeat n "*"))
  )



(defn toggle [a-set elem]
  (if(contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)

  ))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))


(defn old-book->new-book [book]
 (assoc book :authors (set (:authors book)))
  )


(defn has-author? [book author]
  (contains? (book :authors) author)
  )


(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author]
  (if (contains? author :birth-year)
    (str name " (" birth-year " - " death-year ")")
    (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [{:keys [title authors]} book]
    (str title ", written by " (authors->string authors))
    )
  )

(defn books->string [books]
  (let [string (apply str (interpose ". " (map book->string books)))]
    (cond
     (= (count books) 0) "No books."
     (= (count books) 1) (str "1 book. " string ".")
     (> (count books) 1) (str (count books) " books. " string ".")
     )
  ))


(defn books-by-author [author books]
  (filter #(has-author? % author) books)
  )

(defn author-by-name [name authors]
  (first (filter #(= name (% :name)) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true)
  )

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books)
  )

; %________%
