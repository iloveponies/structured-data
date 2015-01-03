(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 _ v2] v]
    (+ v0 v2)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

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
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq))
           (count a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (set (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (str (:name author))
        born (:birth-year author)
        died (:death-year author)
        years (if born
                (str " (" born " - " died ")")
                "")]
    (str author-name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authrs (:authors book)
        authrs-string (authors->string authrs)]
    (str title ", written by " authrs-string)))

(defn books->string [books]
  (let [num-books (count books)
        num-books-string
          (cond
            (== num-books 0) "No books"
            (== num-books 1) "1 book"
            :else (str num-books " books"))
        book-strings (map book->string books)]
    (str (apply str (interpose ". " (cons num-books-string book-strings))) ".")))

(defn books-by-author [author books]
  (let [author-filter (fn [b] (has-author? b author))]
    (filter author-filter books)))

(defn author-by-name [name authors]
  (let [match-name (fn [a] (= name (:name a)))]
    (first (filter match-name authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [alive-authors (living-authors (:authors book))]
    (not (empty? alive-authors))))

(defn books-by-living-authors [books]
  :-)

; %________%
