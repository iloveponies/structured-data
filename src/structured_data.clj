(ns structured-data)

(defn do-a-thing [x]
  (let [doublex (+ x x)]
    (Math/pow doublex doublex)))


(defn spiff [v]
  (+ (get v 0) (get v 2)) )


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
  (let [[point1 point2] rectangle
    [x1 y1] point1
    [x2 y2] point2]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle) ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [px py] point ]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book) ))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))


(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)
    )
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get2 (fn [x] (get x 1) )]
    (map get2 collection)
    ))

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
    ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))


(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
        (assoc book :authors new-authors)))


(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [years (if (contains? author :birth-year)
       (str " ("  (:birth-year author) " - " (:death-year author) ")" ))
        ]
    (str (:name author) years)
    ))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (apply str (interpose ", written by "  [(:title book) (authors->string (:authors book))])))


(defn books->string [books]
  (let [set-books (map book->string books)
        number-books (count set-books)
        rep-count (cond
                    (== number-books 0) "No books"
                    (== number-books 1) "1 book. "
                    :else (str number-books " books. "))
        separeted-books (interpose ". " (map book->string books))]
    ( str rep-count (apply str separeted-books  ) "." )))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


(def authors #{china, felleisen, octavia, friedman})


(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))



; %________%
