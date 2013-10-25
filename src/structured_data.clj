(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)
    )
  )


(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c ] v]
    (+ a c)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (== (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point ]
    (and (<= x1 x x2) (<= y1 y y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [ [[x1 y1] [x2 y2]] inner]
      (and(contains-point? outer (point x1 y1))
          (contains-point? outer (point x2 y2))
        )
    )
  )

(defn title-length [book]
  (count (book :title))
  )

(defn author-count [book]
  (count (book :authors))
  )

(defn multiple-authors? [book]
  (> (count (book :authors)) 1)
  )

(defn add-author [book new-author]
  (let [newAuthors (conj (book :authors) new-author)]
      (assoc book :authors newAuthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [v] (get v 1) )]
      (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq)) )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book) )))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (let [fun (fn [mappi] (:authors mappi) )]
    (apply clojure.set/union (map fun books))))


(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn author->string [author]
  (let [author-name (fn [author] (:name author))
        author-birthyear (fn [author] (:birth-year author))
        author-deathyear (fn [author] (:death-year author))
        years (fn [author] (str " (" (author-birthyear author) " - " (author-deathyear author) ")"  ))]
    (if (contains? author :birth-year)
      (str (author-name author) (years author))
      (str (author-name author)))))


(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  ( str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [numbooks (fn [books] (if (> (count books) 1)
                               (str (count books) " books. ")
                               "1 book. "))]
  (if (empty? books)
    "No books."
    (str (numbooks books) (apply str (interpose ". " (map book->string books))) "." ))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [[author] (filter (fn [x] (= (:name x) name)) authors)]
    author))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (filter (fn [x] (alive? x)) (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
