(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bl_x bl_y] [tr_x tr_y]]]
  (- tr_x bl_x)
  )

(defn height [[[bl_x bl_y] [tr_x tr_y]]]
  (- tr_y bl_y)
  )

(defn square? [[[bl_x bl_y] [tr_x tr_y]]]
  (= (- tr_x bl_x) (- tr_y bl_y))
  )

(defn area [[[bl_x bl_y] [tr_x tr_y]]]
  (* (- tr_y bl_y) (- tr_x bl_x))
  )

(defn contains-point? [[[bl_x bl_y] [tr_x tr_y]] [p_x p_y]]
  (and (<= bl_x p_x tr_x) (<= bl_y p_y tr_y))
  )

(defn contains-rectangle? [outer [ib it]]
  (and
    (contains-point? outer ib)
    (contains-point? outer it)
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (let [new-authors-list (conj (get book :authors) new-author)]
    (conj book {:authors new-authors-list}))
  )

(defn alive? [author]
  (not (boolean (:death-year author)))
  )

(defn element-lengths [collections]
  (map count collections)
  )

(defn second-elements [collections]
  (let [second-element (fn [collection] (get collection 1))]
    (map second-element collections)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
    )
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
  (not= (count (set a-seq)) (count a-seq))
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
  (let [author-name (:name author)
        birth-year (:birth-year author)
        years (if birth-year (str "(" birth-year " - " (:death-year author) ")"))
        ]
    (if birth-year
      (str author-name " " years)
      author-name
      )
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (= (count books) 1) (str "1 book. " (apply str (map book->string books)) ".")
    (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
        (not (empty? (filter alive? authors)))
    )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
