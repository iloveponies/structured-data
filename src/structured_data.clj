(ns structured-data)

(defn do-a-thing [x]
  (let [xpx (+ x x)]
    (Math/pow xpx xpx)))

(defn spiff [v]
  (let [i1 (get v 0)
        i3 (get v 2)]
    (+ i1 i3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[i1 _ i3] v]
    (+ i1 i3)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and
       (<= x1 xp x2)
       (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[pi1 pi2] inner]
    (and
       (contains-point? outer pi1)
       (contains-point? outer pi2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book
    :authors
    (conj
      (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (or
     (apply <= a-seq)
     (apply >= a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
       by (:birth-year author)
       dy (:death-year author)]
    (cond
      by (str name " (" by " - " dy ")")
      :else (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
     (:title book)
     ", written by "
     (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count
          (count books)
        book-count-s
          (cond
             (= book-count 0) "No books"
             (= book-count 1) "1 book. "
             :else (str book-count " books. "))
        book-list
          (apply str (interpose ". " (map book->string books)))
        ]
    (str book-count-s book-list ".")))

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
