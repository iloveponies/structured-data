(ns structured-data)

(defn do-a-thing [x]
  (let [sum-x (+ x x)]
    (Math/pow sum-x sum-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v1 v2 v3] v]
    (+ v1 v3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bl-x bl-y] [tr-x tr-y]] rectangle]
    (- tr-x bl-x)))

(defn height [rectangle]
  (let [[[bl-x bl-y] [tr-x tr-y]] rectangle]
    (- tr-y bl-y)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[min-x min-y] [max-x max-y]] rectangle
        [point-x point-y] point]
    (and (<= min-x point-x max-x) (<= min-y point-y max-y))))

(defn contains-rectangle? [outer inner]
  (let [[inner-p1 inner-p2] inner]
    (and
      (contains-point? outer inner-p1)
      (contains-point? outer inner-p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (if (not new-author) (book)
      (assoc book :authors (conj authors new-author)))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

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
  (let [size-orig (count a-seq)
        size-set (count (set a-seq))]
    (not (== size-orig size-set))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [a-name (:name author)
        a-birth-year (str (:birth-year author))
        a-death-year (str (:death-year author))]
    (if (not (= a-birth-year ""))
      (str a-name " (" a-birth-year " - " a-death-year ")")
      a-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [b-count (count books)
        b-count-str(str
                      (if (== b-count 0) "No" b-count)
                      (if (== b-count 1) " book" " books")
                      (if (== b-count 0) "." ". "))]
    (str b-count-str
         (if (> b-count 0) 
           (str (apply str (interpose ". " (map book->string books))) ".")
           ""))))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (let [author-names (filter (fn [author] (= (:name author) name)) authors)]
    (first author-names)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
