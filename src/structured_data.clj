(ns structured-data)

(defn do-a-thing [x]
    (let [name (+ x x)]
  (Math/pow name name)
  ))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x1 x2) (- y1 y2))
    true
    false
  )))

(defn area [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (* (- x1 x2) (- y1 y2)))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[p1 p2] point]
        (and (<= x1 p1 x2) (<= y1 p2 y2)
  ))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
        (and (contains-point? outer p1) (contains-point? outer p2))
    ))



(defn title-length [book]
  (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
      (assoc book :authors (conj (get book :authors) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [collection] (get collection 1))]
    (map second collection)
    ))

(defn titles [books]
  (let [print-title (fn [book] (get book :title))]
  (let [gobook (fn [books] (map print-title books ))]
    (map print-title books)
    )))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)

    ))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
  )

(defn has-author? [book author]
  (contains? (book :authors) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-name (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-name books)))
    )
  )

(defn author->string [author]
  (let [give-name (:name author)
        birthyear (:birth-year author)
        deathyear (str (:death-year author))
        years (str " (" birthyear " - " deathyear ")")]
    (str give-name (if birthyear years ""))
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
        :else (let [n (count books)
                    plural (if (= 1 n) (str n " book.") (str n " books."))
                    bookstring (fn [book] (str " " (book->string book) "."))]
                (str plural (apply str (apply concat (map bookstring books))))))
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author) ) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))



(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
