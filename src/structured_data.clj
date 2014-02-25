(ns structured-data)

(defn do-a-thing [x]
  (let[y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        raw (- x2 x1)]
    (if (neg? raw) (- raw) raw)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        raw (- y2 y1)]
    (if (neg? raw) (- raw) raw)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let[[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

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
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (update-in book [:authors] set))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (reduce clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (into #{} (map :name (authors books))))

(defn author->string [author]
  (let[dates (if (:death-year author)
               (str " (" (:birth-year author) " - " (:death-year author) ")")
               (if (:birth-year author)
                 (str " (" (:birth-year author) " - )")
                 ""))]
    (str (:name author) dates)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (clojure.string/join (concat (list (:title book) ", written by ") (authors->string (:authors book)))))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (= (count books) 1) (str "1 book. " (book->string (first books)) ".")
   :else (apply str (concat (cons (str (count books) " books. ") (interpose ". " (map book->string books))) ["."]))
   ))


(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter (comp (partial = name) :name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
; %________%
