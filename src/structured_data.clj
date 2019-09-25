(ns structured-data)

(defn do-a-thing [x]
  (let [xsum (+ x x)]
    (Math/pow xsum xsum)))

(defn spiff [v]
  (let [vfirst (get v 0)
        vthird (get v 2)]
    (+ vfirst vthird)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[vfirst vsecond vthird]]
    (+ vfirst vthird))

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
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (let [new-authors (conj authors new-author)]
      (assoc book :authors new-authors))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [book-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map book-authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [a-name (:name author)
        a-byear (:birth-year author)
        a-dyear (:death-year author)]
    (if (contains? author :birth-year)
      (str a-name " (" a-byear " - " a-dyear ")")
      (str a-name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [b-count (count books)
        b-seq (map book->string books)
        b-string (apply str (interpose ", " b-seq))]
    (if (= b-count 0)
      (str "No books.")
      (if (= b-count 1)
        (str b-count " book. " b-string ".")
        (str b-count " books. " b-string ".")))))


(defn books-by-author [author books]
  (let [accept (fn [book] (has-author? book author))]
    (filter accept books)))

(defn author-by-name [name authors]
  (let [accept (fn [a] (= (:name a) name))]
    (first (filter accept authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
