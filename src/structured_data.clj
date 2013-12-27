(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1][x2 y2]] [x3 y3]]
  (and (<= x1 x3 x2) (<= y1 y3 y2)))

(defn contains-rectangle? [outer [bl tr]]
  (and (contains-point? outer bl) (contains-point? outer tr)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)
       ]
  (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [vecs] (get vecs 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [orig-length (count a-seq)
        duplicates-removed-length (count (set a-seq))]
    (not (= orig-length duplicates-removed-length))))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))))

(defn author->string [author]
  (let [name (:name author)
        years (str (:birth-year author)
                   " - "
                   (if (:death-year author)
                     (:death-year author)
                     ""))]
    (str name (if (:birth-year author) (str " (" years ")") ""))))

(defn authors->string [authors]
  (let [name-strings (map author->string authors)]
    (apply str (interpose ", " name-strings))))

(defn book->string [book]
  (let [book-title (:title book)
        authors (:authors book)
        author-string (authors->string authors)]
    (str book-title ", written by " author-string)))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [number-of-books (count books)
          prefix (if (= number-of-books 1) "1 book. " (str number-of-books " books. "))
          book-list (apply str (interpose ". " (map book->string books)))]
      (str prefix book-list "."))))

;30
(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter (fn [a] (nil? (:death-year a))) (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
