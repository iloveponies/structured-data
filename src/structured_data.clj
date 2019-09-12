(ns structured-data)

(defn do-a-thing [x]
    (let [x2 (+ x x)]
        (Math/pow x2 x2)))


(defn spiff [v]
    (+ (or (get v 0) 0)
       (or (get v 2) 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x, y, z]]
(+ (or x 0) (or z 0)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1, y1] [x2, y2]]]
  (Math/abs (- x1, x2)))

(defn height [[[x1, y1] [x2, y2]]]
  (Math/abs (- y1 y2)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1, y1] [x2, y2]] [x3, y3]]
 (and (<= x1 x3 x2) (<= y1 y3 y2)) )

(defn contains-rectangle? [outer [top-left bottom-right]]
  (and (contains-point? outer top-left)
       (contains-point? outer bottom-right)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors
    (conj (:authors book) new-author)]
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [collection] (get collection 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars[n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
    (if (contains? a-set elem)
        (disj a-set elem)
        (conj a-set elem)))


(defn contains-duplicates? [a-seq]
      (< (count (set a-seq))
         (count a-seq)))


(defn old-book->new-book [book]
      (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  (let [book-authors
        (fn [book] (:authors book))]
        (contains? (book-authors book) author)))

(defn authors [books]
  (let [book-authors
        (fn [book]
            (set (:authors book)))]
        (apply clojure.set/union (map book-authors books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
      (let
           [name (fn [author] (:name author))]
           (let
               [years (fn [author]
                          (str " ("(:birth-year author) " - "
                               (:death-year author) ")"))]
             (str (name author) (if (contains? author :birth-year)
                                    (years author) "")))

        ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
      (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count-str (fn [books]
                         (cond (empty? books) "No books."
                               (== (count books) 1) "1 book. "
                               :else (str (count books) " books. ")))
        book->string1 (fn [book] (str (book->string book) (if (> (count books) 1) ". " ".")))]
        (apply str (book-count-str books) (map book->string1 books))))




(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
      (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
      (let [authors (fn [book] (:authors book))]
           (not (empty? (filter alive? (authors book))))))

(defn books-by-living-authors [books]
      (filter has-a-living-author? books))

; %________%
