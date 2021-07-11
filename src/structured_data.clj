(ns structured-data)

(defn do-a-thing [x]
  (let [doub (+ x x)]
    (Math/pow doub doub)))

(defn spiff [v]
  (let [a (first v)
        b (nth v 2)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner
        point1 [x1 y1]
        point2 [x1 y2]
        point3 [y1 x1]
        point4 [y1 y2]]
    (and (contains-point? outer point1)
         (contains-point? outer point2)
         (contains-point? outer point3)
         (contains-point? outer point4))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1 ))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [get-sec (fn [coll] (get coll 1))]
    (map get-sec collection)))

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq)))
)

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (seq (authors books))))
)

(defn author->string [author]
  (let [name (:name author)
        years (if (:birth-year author) 
               (str " (" (:birth-year author) " - " (:death-year author) ")")
                    "")]
    (str name years)
   )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [title (:title book)
        authors-string (authors->string (:authors book))]
    (apply str (interpose ", written by " [title authors-string])))
)

(defn books->string [books]
  (let [nbr (count books)
        nbr-label (case nbr
                   0 "No books"
                   1 "1 book. "
                   (str nbr " books. "))
        book-str (apply str (interpose ". " (map book->string books)))]
    (apply str nbr-label book-str "."))
)

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)) 
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  "takes a sequence of books as a parameter and 
   returns those that have a living author"
  [books]
  (filter (fn [book] (has-a-living-author? book)) books)
)

; %________%
