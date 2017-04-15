(ns structured-data)

(defn do-a-thing [x]
  (let [x1 (+ x x)]
  (Math/pow x1 x1)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z][(get v 0) (get v 1) (get v 2)]]
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
      (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (== (- y2 y1) (- x2 x1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (* (- y2 y1) (- x2 x1))))

(defn contains-point? [rectangle point]
  (let [[x3 y3] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
          (and (<= x1 x3 x2) (<= y1 y3 y2)) )))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 1))
       (contains-point? outer (get inner 0))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let[authors (get book :authors)]
    (let[new-authors (conj authors new-author)]
      (assoc book :authors new-authors))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
    (let [sequencer (fn [x] (get x 1) )]
      (map sequencer collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (apply concat(repeat n (str "*")))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (== (count a-seq) (count a-set)))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-set (fn [book] (set (:authors book)))]
    (apply clojure.set/union (map author-set books))))

(defn all-author-names [books]
  (let [name-seq (map :name (authors books))]
    (set name-seq))
  )

(defn author->string [author]
  (let [birth (:birth-year author)
        death (:death-year author)
        author (:name author)]
    (str author
         (if birth
           (str " (" birth " - "
                (if death
                  (str death ")")
                  ")"))
           ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let [book-info (apply str (interpose ", " (map book->string books)))
        book-num (count books)]
    (str
      (cond
         (== book-num 0)
           "No books"
         (== book-num 1)
           "1 book. "
         :else
           (str book-num " books. "))
     book-info "."
  )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
