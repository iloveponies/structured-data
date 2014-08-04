(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)] (
    Math/pow xx xx
    )))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)
  )
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [ [x y z] ] 
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [ [ [x1 y1] [x2 y2] ] ]
  ( Math/abs (- x1 x2)))

(defn height [ [ [x1 y1] [x2 y2] ] ]
  ( Math/abs (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[ [x1 y1] [x2 y2] ] [x3 y3] ]
  (and (<= x1 x3 x2 ) (<= y1 y3 y2 ) ))

(defn contains-rectangle? [outer [ p1 p2 ]]
  (and (contains-point? outer p1) (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  ( assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [v] (get v 1))]
    (seq (map sec collection))))

(defn titles [books]
  (seq (map :title books)))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (:birth-year author) (str " (" (:birth-year author) " - " (:death-year author) ")") nil)]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [ len (count books) ]
    (cond
      (== len 0) "No books."
      (== len 1) (str "1 book. " (book->string (get books 0)) ".")
      :else (str len " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [fun (fn [x] (has-author? x author))]
  (filter fun books)))

(defn author-by-name [name authors]
  (let [fun (fn [x] (= name (:name x)))]
  (first (filter fun authors))))

(defn living-authors [authors]
  (let [fun (fn [x] (alive? x))]
    (filter fun authors)))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
