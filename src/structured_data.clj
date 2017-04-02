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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

; later - fix this
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
       (and (<= x1 px x2) (<= y1 py y2)))
)

(defn contains-rectangle? [outer inner]
  (let [[inner-bl inner-tr] inner]
      (and (contains-point? outer inner-bl) (contains-point? outer inner-tr)))
)

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
    (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)
  new-authors (conj authors new-author)]
  (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
    (let [get-second (fn [item] (get item 1))]
        (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (apply concat (repeat n "*"))))

(defn toggle [a-set elem]
    (cond
        (contains? a-set elem) (disj a-set elem)
        :else (conj a-set elem)
        ))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
    (cond
        (contains? author :death-year) (apply str [(get author :name) " (" (get author :birth-year) " - " (get author :death-year) ")"])
        (contains? author :birth-year) (apply str [(get author :name) " (" (get author :birth-year) " - )"])
        :else (str (get author :name))
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(get book :title) ", written by " (authors->string (get book :authors))]))

(defn books->string [books]
    (let [book-count (count books)]
    (cond
        (= book-count 0) "No books."
        (= book-count 1) (apply str["1 book. " (book->string (get books 0)) "."])
        :else (apply str[book-count " books. " (apply str (interpose ". " (map book->string books))) "."])
    )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
    (let [matches (filter (fn [author] (= (get author :name) name)) authors)]
    (cond
        (empty? matches) nil
        :else (first matches)
        ))
    )

(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
