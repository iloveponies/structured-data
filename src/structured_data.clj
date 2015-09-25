(ns structured-data)

(defn do-a-thing [x]
  (let [double-arg (+ x x)]
    (Math/pow double-arg double-arg)))

(defn spiff [v]
  (let [vector-first (get v 0)
        vector-third (get v 2)]
    (+ vector-first vector-third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[vector-first vector-second vector-third] v]
    (+ vector-first vector-third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [w (- x2 x1)]
      (if (> w 0)
        w
        (* w -1)))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [h (- y2 y1)]
      (if (> h 0)
        h
        (* h -1)))))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and
      (<= x1 xp x2)
      (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
      (contains-point? outer p1)
      (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not
    (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [v] (get v 1))]
    (map get-second collection)))

(defn titles [books]
  (let [get-titles (fn [m] (:title m))]
    (map get-titles books)))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

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
  (let [author-name (:name author)
        author-birth (:birth-year author)
        author-death (:death-year author)]
    (cond
      author-birth (str author-name " (" author-birth " - " author-death ")")
      :else (str author-name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book)
        authors (authors->string (:authors book))]
    (str book-title ", written by " authors)))

(defn books->string [books]
  (let [num-books (count books)
        num-books-string (cond
                           (= num-books 0) "No books"
                           (= num-books 1) "1 book. "
                           :else (str num-books " books. "))
        list-books (apply str (interpose ". " (map book->string books)))]
    (str num-books-string list-books ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
