(ns structured-data)

(defn do-a-thing [x]
  (let [yy (+ x x)]
    (Math/pow yy yy))
  )

(defn spiff [v]
    (+ (get v 0) (get v 2))
  )

(defn cutify [v]
    (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
  )
  )

(defn point [x y]
  [x y]
  )

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
  (let [[[x1 y1] [x2 y2]] rectangle]
        (==
          (- x1 x2) (- y1 y2))
        )
    )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (*
          (- x2 x1) (- y2 y1))
        )
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
                [x3 y3] point]
        (and (<= x1 x3 x2) (<= y1 y3 y2))
        )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] inner]
        (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4]))
        )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (count (:authors book)))
  )

(defn add-author [book new-author]
  (let [book-authors (:authors book)]
      (assoc book :authors (conj book-authors new-author))
  ))

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
    (let [secondz (fn [col] (get col 1))]
           (seq (map secondz collection)))
  )

(defn titles [books]
  (let [titlez (fn [book] (:title book))]
    (seq (map titlez books))
    )
  )

(defn monotonic? [a-seq]
    (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
    (if (< (count (set a-seq)) (count a-seq)) true false)
  )

(defn old-book->new-book [book]
    (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
    (contains? (:authors book) author)
  )

(defn authors [books]
  (let [authorz
        (fn [book] (clojure.set/union (:authors book)))]
    (set (apply clojure.set/union (map authorz books)))
    )
  )

(defn all-author-names [books]
    (set (map :name (authors books)))
  )

(defn author->string [author]

  (let [name (:name author)
                birth (:birth-year author)
                death (:death-year author)]
        (if (= nil death)
                 (if (= nil birth)
                            (str name)
                            (str name " (" birth " - " ")")
                          )
                 (str name " (" birth " - " death ")")
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
        (== (count books) 0) (str "No books.")
        (< 1 (count books)) (str (count books) " books. " (apply str (interpose ". "(map book->string books))) ".")
        :else (str "1 book. " (apply str (map book->string books)) ".")
        )
  )

(defn books-by-author [author books]
  (let [predicate
        (fn [book] (has-author? book author))]
        (filter predicate books)
        )
  )

(defn author-by-name [name authors]
  (let [predicate
          (fn [author] (= (:name author) name))]
        (first (filter predicate authors))
        )
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