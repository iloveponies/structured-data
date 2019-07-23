(ns structured-data)

(comment "Specific data to define in the REPL"
         (def china {:name "China Miéville", :birth-year 1972})
         (def octavia {:name "Octavia E. Butler"
                       :birth-year 1947
                       :death-year 2006})
         (def friedman {:name "Daniel Friedman" :birth-year 1944})
         (def felleisen {:name "Matthias Felleisen"})

         (def cities {:title "The City and the City" :authors[china]})
         (def wild-seed {:title "Wild Seed", :authors [octavia]})
         (def embassytown {:title "Embassytown", :authors [china]})
         (def little-schemer {:title "The Little Schemer"
                              :authors [friedman, felleisen]})

         (def books [cities, wild-seed, embassytown, little-schemer])
         (def authors #{china, felleisen, octavia, friedman})

         (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
         (def christopher {:name "Christopher Tolkien" :birth-year 1924})
         (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

         (def silmarillion {:title "Silmarillion"
                            :authors #{jrrtolkien, christopher, kay}}))


(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (v 0) (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        d (- y2 y1)]
    (cond
     (neg? d) (- d)
     :else d)))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        d (- x2 x1)]
    (cond
     (neg? d) (- d)
     :else d)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2)
         (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]]  inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
   (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author )]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map #(count %) collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [increasing? (fn [x] (apply <= a-seq))
        decreasing? (fn [x] (apply >= a-seq))]
    (or (increasing? a-seq) (decreasing? a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond (contains? a-set elem) (disj a-set elem)
        :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (> (count (vec a-seq)) (count a-set))))

(defn old-book->new-book [book]
  (let [authors (into #{} (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? (set authors) author)))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        death-year (:death-year author)
        birth-year (:birth-year author)]
    (cond (or birth-year death-year)
          (apply str name " (" (:birth-year author) " - " (:death-year author) ")")

          :else (apply str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors (:authors book)
        title (:title book)]
    (cond title (apply str title ", written by " (authors->string authors)))))

(defn books->string [books]
  (let [num (count books)
        books-string (apply str (interpose ", " (map book->string books)))
        ]
    (cond (= 0 num) (apply str "No books.")
              (= 1 num) (apply str "1 book. " books-string ".")
              (< 1 num) (apply str num " books. " books-string  "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn author-by-name2 [name authors]
  (filter #(= :name name) authors))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
; %________%
