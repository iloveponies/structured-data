(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2))
)


(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
  )
)

(defn point [x y]
  [x y]
)

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
)

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] do
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] do
  (- y2 y1))
)

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] do
  (= (height rectangle) (width rectangle)))
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] do
    (* (height rectangle) (width rectangle))
    )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [px py] point ] do
    (and (<= x1 px x2) (<= y1 py y2))
   )
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner] do
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2])
         )
    )
)

(defn title-length [book]
  ;(def cities {:title "The City and the City" :authors [china]})
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  ;(println "aut count" (author-count book) (> (author-count book) 1))
  (> (author-count book) 1 )
)

(defn add-author [book new-author]
  (let [old-author (:authors book)]
    ;l'ordine in conj e importante. aggiungo un elemento ad un vettore-non viceversa
    ;mettere come return l'ultima espressione
    ;assoc non modifica book, crea una copia con la modifica e la restituisce
    (assoc book :authors (conj old-author new-author))
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [get-second (fn[x] (get x 1))]
    (map get-second collection)
  )
)


(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
  (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))) )
  )

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)
  )
)

(defn has-author? [book author]
  ;(println "ha: " (set (map :name (:authors book))) )
  ;(println "auth" (:name author))
  (contains? (set (map :name (:authors book))) (:name author))
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))
  )
)

(defn author->string [author]
  (let [my-year (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (let [[nname year] [(:name author) (if (contains? author :birth-year) my-year "" )]]
        (str nname year)
    )
  )
)

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors)))
)

(defn book->string [book]
  ;(println (:authors book))
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (let [book-count (count books)]
    ;(println book-count)
    (cond
      (= 0 book-count) "No books."
      (= 1 book-count) (str "1 book. " (book->string (get books 0)) "." )
      :else (str book-count " books. " (apply str (interpose ". " (map book->string books)) ) "." )
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors))
)

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
)

