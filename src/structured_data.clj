(ns structured-data)

(defn do-a-thing [x]
  (let [ x+x (+ x x)]
    (Math/pow x+x x+x)))

(defn spiff [v]
  (if (> (count v) 2)
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (> (count v) 2)
    (let [[x0 x1 x2] v]
      (+ x0 x2))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[ [x-bl y-bl] [x-tr y-tr] ] rectangle]
    (- x-tr x-bl)))

(defn height [rectangle]
  (let [[ [x-bl y-bl] [x-tr y-tr] ] rectangle]
    (- y-tr y-bl)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[ [x-bl y-bl] [x-tr y-tr] ] rectangle
        [x y] point]
    (and (<= x-bl x x-tr) (<= y-bl y y-tr))))

(defn contains-rectangle? [outer inner]
  (let [ [pt-bl pt-tr] inner]
    (and (contains-point?  outer pt-bl)
         (contains-point? outer pt-tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element #(get % 1)]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (into #{} (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        yrs (cond
             (:death-year author) (str " (" (:birth-year author) " - " (:death-year author) ")")
             (:birth-year author) (str " (" (:birth-year author) " - " ")")
             :else "")]
    (str name yrs)))

(defn authors->string [authors]
  (apply str
         (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [book-num (count books)
          books-info
          (into [(str book-num " book" (if (> book-num 1) "s" ""))]
                (map book->string books))]
      (str
       (apply str (interpose ". " books-info))
       "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

