(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
    (and (== (width rectangle) (height rectangle))))


(defn area [rectangle]
    (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point ]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))


(defn contains-rectangle? [outer inner]
  (let [[[a1 b1] [a2 b2]] inner]
    (and (contains-point? outer [a1 b1]) (contains-point? outer [a2 b2]))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))


(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [findsec (fn [v] (get v 1))]
    (map findsec collection)))

(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))



(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  (contains? (:authors book) author))



(defn authors [books]
  (let [getAuthors (fn [books] (map :authors books))]
    (apply clojure.set/union (getAuthors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (fn [v] (:name author))
        year (fn [c] (cond
                        (contains? author :death-year) (str " (" (:birth-year author) " - "(:death-year author) ")")
                        (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                        :else ""))]
    (str (name author) (year author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [counter (fn [v] (cond
                          (< 1 (count v)) (str (count v) " books. ")
                          (== 1 (count v)) (str (count v) " book. ")
                          :else "No books"))]
    (str (counter books) (apply str (interpose ". " (map book->string books))) ".")))


(defn books-by-author [author books]
  (filter (fn [book]
            (if (contains? (:authors book) author)
              true
              false)) books))


(defn author-by-name [name authors]
   (first (filter (fn [author]
            (if (= name (:name author))
                         true
                         false)) authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
