(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
  (let [f (or (get v 0) 0)
        t (or (get v 2) 0)]
    (+ f t)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v
        x1 (or x 0)
        z1 (or z 0)]
    (+ x1 z1)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(width [[1 1] [5 1]])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(height [[1 1] [5 1]])

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x1 y2))
         (contains-point? outer (point x2 y1))
         (contains-point? outer (point x2 y2)))))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
    (assoc book :authors (conj (:authors book) new-author)))

(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [col] (get col 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(monotonic? [1 2 3])

(defn stars [n]
  (apply str (repeat n \*)))

(stars 10)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true

(defn old-book->new-book [book]
  (let [authors-map (set (:authors book))]
    (assoc book :authors authors-map)))

(old-book->new-book
  {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
            "British Science Fiction Award"]
   :title "The City and the City"
   :authors [{:birth-year 1972, :name "China Miéville"}]})

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))


(defn authors [books]
  (let [new-books (map old-book->new-book books)
        authors  (map :authors new-books)]
    (apply clojure.set/union authors)))


(defn all-author-names [books]
  (let [authors (authors books)]
    (set (map :name authors))))


(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
       birth (str name " (" birth " - " death ")")
       :else name)))


(defn authors->string [authors]
  (let [authorss (map author->string authors)]
    (apply str (interpose ", " authorss))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (= 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else (apply str [(count books)
                      " books. "
                      (apply str (interpose ". " (map book->string books)))
                      "."])))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))



(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%


