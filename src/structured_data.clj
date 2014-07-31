(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x)))

(defn spiff [v]
  (+ (v 0) (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ b]]
  (+ a b))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[blx bly] [trx try]]]
  (- trx blx))

(defn height [[[blx bly] [trx try]]]
  (- try bly))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [outer [bl tr]]
  (and (contains-point? outer bl) (contains-point? outer tr)))

(defn title-length [book]
  (-> book :title count))

(defn author-count [book]
  (-> book :authors count))

(defn multiple-authors? [book]
  (-> book author-count (> 1)))

(defn add-author [book new-author]
  (assoc book :authors (-> book :authors (conj new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (->> books authors (map :name) set))

(defn author->string [author]
  (str
    (:name author)
    (if (:birth-year author)
	(str " (" (:birth-year author) " - " (:death-year author) ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)]
    (cond
	(== book-count 0) "No books."
	:else (str
		book-count
		" book"
		(if (> book-count 1) "s")
		". "
		(apply str (interpose " " (map #(-> % book->string (str \.)) books)))))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (->> authors (filter #(= (:name %) name)) first))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (boolean (some #(not (:death-year %)) (:authors book))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
