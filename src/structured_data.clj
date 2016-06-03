(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+
   (v 0)
   (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[e1 e2 e3] v]
    (+ e1 e3)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and
     (<= x1 x x2)
     (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
     (contains-point? outer p1)
     (contains-point? outer p2))))

(defn title-length [book]
  (count
   (:title book)))

(defn author-count [book]
  (count
   (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        enriched-authors (conj authors new-author)]
    (assoc book :authors enriched-authors)))

(defn alive? [author]
  (not  (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not
   (=
    (count a-seq)
    (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book
    :authors (set (:authors book))))

(defn has-author? [book author]
  (let [authors (:authors (old-book->new-book book))]
    (contains? authors author)))

(defn authors [books]
  (let [book-authors (fn [book] (:authors (old-book->new-book book)))]
    (apply clojure.set/union
           (map book-authors books))))

(defn all-author-names [books]
  (into #{}  (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [caption (cond
                 (= 0 (count books)) "No books."
                 (= 1 (count books)) "1 book. "
                 :else (str (count books) " books. "))
        book->str-with-dot (fn [book] (str (book->string book) "."))
        books-desc (apply str (interpose  " " (map book->str-with-dot books)))]
    (str caption books-desc)))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [name-matches (fn [author] (= name (:name author)))]
    (first
     (filter name-matches authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
