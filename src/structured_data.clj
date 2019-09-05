(ns structured-data)

(defn do-a-thing [x]
  (let [sum-of-x (+ x x)]
    (Math/pow sum-of-x sum-of-x)))

(defn spiff [v]
  (let [x (get v 0)
        z (get v 2)]
    (+ x z)))

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
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (- x2 x1)))

(defn height [rectangle]
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and
      (contains-point? outer inner-bottom-left)
      (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [my-second (fn [x] (get x 1))]
    (map my-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-count (count a-seq)
        set-count (count (set a-seq))]
    (> seq-count set-count)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [books-authors (map :authors books)]
    (apply clojure.set/union books-authors)))

(defn all-author-names [books]
  (let [books-authors (authors books)]
    (set (map :name books-authors))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
      death (str name " (" birth " - " death ")")
      birth (str name " (" birth " - )")
      :else name)))

(defn authors->string [authors]
  (let [authors-strings (map author->string authors)]
    (apply str (interpose ", " authors-strings))))

(defn book->string [book]
  (let [book-title (:title book)
        authors (authors->string (:authors book))]
    (str book-title ", written by " authors)))

(defn books->string [books]
  (let [books-number (count books)
        books-string (map book->string books)
        books-summary (if (= books-number 1) "1 book" (str books-number " books"))
        full-summary (interpose ". " (conj books-string books-summary))]
    (if (seq books)
      (str (apply str full-summary) ".")
      "No books.")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%