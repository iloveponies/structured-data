(ns structured-data)

(defn do-a-thing [x]
    (let [xx (+ x x)]
  (Math/pow xx xx)))


(defn spiff [v]
   (let [first (get v 0)
        second (get v 2) ]
    (+ first second)))

(defn cutify [v]
 (conj v "<3"))

(defn spiff-destructuring [v]
   (let [first (get v 0)
        second (get v 2) ]
    (+ first second)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn square? [[[x1 y1] [x2 y2]]]
  (if (= (- x1 x2)(- y1 y2)) true false
  ))

(defn area [[[x1 y1] [x2 y2]]]

  (* (- x1 x2) (- y1 y2)))

(defn contains-point? [[[x1 x2] [y1 y2]][z1 z2]]
  (if (<= x1 z1 y1) (if (<= x2 z2 y2) true false) false))


(defn contains-rectangle?  [[[x1 x2] [y1 y2]][[z1 z2][k1 k2]]]
  (if (<= x1 z1 k1 y1) (if (<= x2 z2 k2 y2) true false) false))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1) true false))

(defn add-author [book new-author]
 (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
(defn mapelement-lengths [collection]
  (count collection))
  (map mapelement-lengths collection))

 (defn second-elements [collection]
    (defn mapsecond-elements [collection]
    (get collection 1))
    (map mapsecond-elements collection))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (<=(get a-seq 0)(get a-seq 1))
    (if (<=(get a-seq 1)(get a-seq 2)) true false )
    (if (>=(get a-seq 1)(get a-seq 2)) true false))
 )

(defn stars [n]
(apply str (repeat n \*)))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]

  (if (= (count a-seq) (count (set a-seq))) false true)
 )

(defn old-book->new-book [book]
   (assoc book :authors (set (:authors book)))

  )

(defn has-author? [book author]
  (if (contains? (get book :authors) author) true false))


(defn authors [books]
  (let [author-names
         (fn [book] (get book :authors))]
    (set (apply concat (map author-names books)))))


(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
 (str (get author :name) (if (nil? (:birth-year author)) "" (str " (" (get author :birth-year) " - " (get author :death-year) ")") )
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
 (str (get book :title) ", written by " (authors->string (:authors book))))

(defn books->string [books]
 (str (if (= (count books) 0) (str "No books")
   (if (= (count books) 1) (apply str "1 book. "
   (interpose ", " (map book->string books)))
   (apply str (count books)" books. " (interpose ", " (map book->string books)))))
  "."))

(defn books-by-author [author books]
(filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
 (if (empty? (filter (fn [b] (= (get b :name) name)) authors)) nil (first (filter (fn [b] (= (get b :name) name)) authors))))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (= 0 (count(filter alive? (get book :authors)))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
