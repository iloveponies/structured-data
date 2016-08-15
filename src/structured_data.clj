(ns structured-data)

(defn do-a-thing [x]
    (let [x2 (+ x x)]
  (Math/pow x2 x2)))

(defn spiff [v]
    (+ (get v 0) (get v 2)))

(defn cutify [v]
    (conj v "<3"))

(defn spiff-destructuring [v]
    (let [[x y z] v] (+ x z)))

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
    (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1)))
)

(defn area [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (contains-point? outer [x3 y3])
         (contains-point? outer [x4 y4]))))

(defn title-length [book]
    (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (count (get book :authors))))

(defn add-author [book new-author]
  (let [a (:authors book)]
    (assoc book :authors (conj a new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [poimi2 (fn [v] (get v 1))]
    (map poimi2 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
    (if (contains? a-set elem)
        (disj a-set elem)   ;true
        (conj a-set elem))) ;false

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [a (:authors book)]
    (assoc book :authors (set a))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (seq (authors books)))))

(defn author->string [author]
    (let [nimi (fn [author] (str (:name author)))
        vuodet (fn [author]
                 (cond (contains? author :death-year)
                     (str " (" (:birth-year author) " - " (:death-year author) ")")
                   (contains? author :birth-year)
                       (str " (" (:birth-year author) " - )")
                    :else ""))]
    (str (nimi author) (vuodet author))))

(defn authors->string [authors]
  (apply str
    (interpose ", "
      (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
    (cond (== (count books) 0) "No books."
          (== (count books) 1)   (str "1 book. "
                                 (apply str
                                  (interpose ". "
                                   (map book->string books))))
          :else   (str (count books)
                     " books. "
                     (apply str
                      (interpose ". "
                        (map book->string books))))
        ))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
