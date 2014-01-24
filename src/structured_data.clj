(ns structured-data)

(defn do-a-thing [x]
  (let [add-x (+ x x)]
    (Math/pow add-x add-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x0 _ x2]]
  (+ x0 x2))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (- xtr xbl)))

(defn height [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (- ytr ybl)))

(defn square? [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (== (- ytr ybl) (- xtr xbl))))

(defn area [rectangle]
  (let [[[xbl ybl] [xtr ytr]] rectangle]
    (* (- ytr ybl) (- xtr xbl))))

(defn contains-point? [rectangle point]
  (let [[[xbl ybl] [xtr ytr]] rectangle
        [xp yp] point]
    (and (<= xbl xp xtr) (<= ybl yp ytr))))

(defn contains-rectangle? [outer inner]
  (let [[[ixbl iybl] [ixtr iytr]] inner]
    (and (contains-point? outer [ixbl iybl]) (contains-point? outer [ixtr iytr]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [authors-with-new (conj (:authors book) new-author)]
    (assoc book :authors authors-with-new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [a-collection] (get a-collection 1))]
    (map second-element collection)))

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
  (contains? (set (map :name (:authors book))) (:name author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years [(:birth-year author), (:death-year author)]]
    (str name (if (not (empty? (filter (fn [x] (not (nil? x))) years)))
                (str " (" (first years) " - " (second years) ")")
                ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [books-count-msg (cond
                          (zero? (count books)) "No books"
                          (== 1 (count books)) "1 book. "
                          (> (count books) 1) (str (count books) " books. "))
        books-vector (map book->string books)]
      (str books-count-msg (apply str (interpose ". " books-vector)) ".")))

(defn books-by-author [author books]
  (filter (fn [book]
            (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
