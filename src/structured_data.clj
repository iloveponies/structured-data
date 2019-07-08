(ns structured-data)

(defn do-a-thing [x]
  (let [blah (+ x x)]
    (Math/pow blah blah)))

(defn spiff [v]
  (when-not (empty? v)
    (+ (nth v 0) (nth v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _][x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1][_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[xl yl] [xr yr]] [xp yp]]
  (and (<= xl xp xr) (<= yl yp xr)))

(defn contains-rectangle? [outer [ip1 ip2]]
  (and (contains-point? outer ip1) (contains-point? outer ip2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        authors (conj old-authors new-author)]
    (assoc book :authors authors)))

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
  (apply str (repeat n \*)))

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
        birth (:birth-year author)
        death (:death-year author)
        years (if (nil? birth)
                ""
                (str " (" birth " - " death ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (format "%s, written by %s" title authors)))

(defn books->string [books]
  (let [book-strings (map book->string books)
        book-count (count books)
        summary (cond
                 (= book-count 0) "No books"
                 (= book-count 1) "1 book"
                 :else (format "%s books" book-count))]
    (str (apply str (interpose ". " (cons summary book-strings))) ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [author (filter #(= name (:name %)) authors)]
    (if (empty? author)
      nil
      (first author))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
