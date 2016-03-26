(ns structured-data)

(defn do-a-thing [x]
  (let [sumBySelf (+ x x)]
  (Math/pow sumBySelf sumBySelf)))

(defn spiff [v]
  (if (< (count v) 3 )
    nil
    (+ (get v 0) (get v 2))
  ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
  (+ a c)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 (first point) x2) (<= y1 (second point) y2) )))

(defn contains-rectangle? [outer inner]
  (and
    (contains-point? outer (first inner))
    (contains-point? outer (second inner))
  ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))


(defn add-author [book new-author]
  (let [new-authors  (conj (:authors book) new-author)]
  (assoc book :authors new-authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (let [el-length (fn [el] (count el))]
    (map el-length collection)))

(defn second-elements [collection]
  (map (fn [arr] (get arr 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [ name (:name author)
         year (str
           (cond
             (:death-year author)
               (str " (" (:birth-year author) " - " (:death-year author) ")")
             (:birth-year author)
               (str " (" (:birth-year author) " - )")
             :else ""
            ))
         ]
    (str name year)
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (str (count books))
        books-str (apply str (interpose ". " (map book->string books)))
        ]
  (cond
    (= 0 (count books))
      "No books."
    (= 1 (count books))
      (str book-count " book. " books-str ".")
    :else
      (str book-count " books. " books-str ".")
    )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
