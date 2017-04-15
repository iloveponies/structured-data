(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

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
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [{authors :authors} book
        all-authors (conj authors new-author)]
    (assoc book :authors all-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [munge (fn [x] (second x))]
    (map munge collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

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
        birth (:birth-year author)
        death (:death-year author)
        years (str " (" birth " - " death ")")]
    (str name (if (contains? author :birth-year) years))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (authors [book]))))

(defn books->string [books]
  (let [book-count (count books)
        book-list (apply str (interpose ". " (map book->string books)))]
    (cond
      (== book-count 0) "No books."
      (== book-count 1) (str book-count " book. " book-list ".")
      :else (str book-count " books. " book-list "."))))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (let [same-name? (fn [author] (= (:name author) name))
        author (filter same-name? authors)]
    (if (empty? author) nil (first author))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (contains? (set (map alive? (:authors book))) true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
