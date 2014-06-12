(ns structured-data)

(defn do-a-thing [x]
  (let  [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff [v]
  (let [v-first (get v 0)
        v-third (get v 2)]
    (+ v-first v-third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map #(:title %) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply <= (reverse a-seq))))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq))) true false))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (or (:death-year author) "")
        time-string (if birth-year (str " (" birth-year " - " death-year ")") "")]
    (str name time-string)))

(defn authors->string [authors]
  (let [author-set (set (map author->string authors))]
    (apply str (interpose ", " author-set))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [num-books (count books)
        book-tag (if (> num-books 1) " books" " book")]
    (if (zero? num-books) "No books."
        (str (apply str (interpose ". " (cons (str num-books book-tag) 
                                              (map book->string books)))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors )))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
    (if-not (empty? (living-authors (:authors book))) true false))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
