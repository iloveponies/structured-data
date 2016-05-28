(ns structured-data)

(defn do-a-thing [x]
  (let [doubleEx (+ x x)]
    (Math/pow doubleEx doubleEx)))

(defn spiff [v]
  (+
    (get v 0)
    (get v 2)))



(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]] (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle] (= (width rectangle) (height rectangle))
  )

(defn area [rectangle] (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1] [rx2 ry2]] rectangle
        [px py] point]
    (and
      (<= rx1 px rx2)
      (<= ry1 py ry2))))

(defn contains-rectangle? [outer inner]
  (let [[bottomLeft topRight] inner]
    (and
      (contains-point? outer bottomLeft)
      (contains-point? outer topRight))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors
              (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [[a b]] b)]
    (map second-element collection))
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (let [stars-seq (repeat n \*)]
    (apply str stars-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-len (count a-seq)
        set-len (count (set a-seq))]
    (not= seq-len set-len)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author ))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name-part (:name author)
        death-year-part (or (:death-year author) "")
        birth-year (:birth-year author)
        years-part (or (and birth-year (str " (" birth-year " - " death-year-part ")")) "")]
    (str name-part years-part)
    ))

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)
        book-descriptions (map book->string books)
        book-descriptions-separated (interpose ". " book-descriptions)
        book-descriptions-part (apply str book-descriptions-separated)]
        (cond
                          (= 0 book-count) "No books."
                          (= 1 book-count) (str "1 book. " book-descriptions-part ".")
                          :else (str book-count " books. " book-descriptions-part "." ))
    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter
           (fn [author] (= (:name author) name))
           authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
