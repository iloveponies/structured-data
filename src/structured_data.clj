(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [ [a b c]  v ]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
  (- x2 x1)))

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
  (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle
         [px py]           point]
    (if (and (<= x1 px x2) (<= y1 py y2) ) true false)))

(defn contains-rectangle? [outer inner]
  (let [ [[x1 y1] [x2 y2] ]  inner]
    (if (and (contains-point? outer (point x1 y1))
             (contains-point? outer (point x2 y2))) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
      (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let[authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)  (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (< (count a-set) (count a-seq))
     true false)))

(defn old-book->new-book [book]
  (let [authors-set (set(:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (set(apply concat(map :authors books))))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name   (:name author)
        b-year (:birth-year author)
        d-year (:death-year author)]
    (if (contains? author :birth-year)
      (str name " (" b-year " - " d-year ")")
      name)))

(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [count-string (if ( > (count books) 1) (str (count books) " books.") "1 book.")]
    (apply str (interpose ". " (map book->string books)))))

(defn books->string [books]
  (let [b-count (count books)]
  (cond
    (== b-count 0) "No books."
    (== b-count 1) (str "1 book. " (book->string (first books)) ".")
    :else (str b-count " books. "(apply str (interpose ", " (map book->string books))) "." ))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter  #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
    (not(empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%

;(> (count [1 2]) 1)

;(contains? (seq [1 2 3]) 3)
