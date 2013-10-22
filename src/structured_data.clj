(ns structured-data)

(defn do-a-thing [x]
  (let [lela (Math/pow (+ x x) (+ x x))] lela)
  )

(defn spiff [v]
  (if (empty? v) "?" (+ (get v 0) (get v 2))
  )
)
(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[b u p] v] (+ (get v 0) (get v 2)))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
  (and (<= x1 p1 x2) (<= y1 p2 y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner] (and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (<= 2 (author-count book)))

(defn add-author [book new-author]
  (let[newauthors (conj (:authors book) new-author)] (assoc book :authors newauthors))  
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [s (fn [v] (get v 1))] (map s collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [oldbook book]) (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [y 
    (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")") 
                nil)]
  (str (:name author) y)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (= 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (let [author-has-book? (fn [b] (has-author? b author))]
    (filter author-has-book? books)))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name))authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
