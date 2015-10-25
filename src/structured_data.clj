(ns structured-data)

(defn do-a-thing [x]
  (let [ xs (+ x x) ] 
    (Math/pow xs xs))
  )

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)
        ]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [ [x y z] v]
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
  (let [width (width rectangle)
        height(height rectangle)]
    (== width height)))

(defn area [rectangle]
  (let [width (width rectangle)
        height(height rectangle)]
    ( * width height)))

(defn contains-point? [rectangle point]
  (let [[[sx1 sy1] [sx2 sy2]] rectangle
        [px py] point]
    (and (<= sx1 px sx2) (<= sy1 py sy2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) 
         (contains-point? outer point2))))

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])

(def books [cities, wild-seed, embassytown, little-schemer])

(defn title-length [book]
  (let [title (:title book)]
    (count title)))

(defn author-count [book]
  (let [authors (:authors book)]
    (count authors)))

(defn multiple-authors? [book]
  (if ( < 1 (author-count book))
      true
      false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
  (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ elems (fn [element] (get element 1)) ]
  (map elems collection)))

(defn titles [books]
  (let [title (fn [book] (book :title))]
    (map title books)))

(defn monotonic? [a-seq]
  (apply <= a-seq))

(defn stars [n]
  (let [starList (fn [n] (repeat n "*"))]
    (apply str (starList n))))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [setseq (set a-seq)]
    (not (== (count setseq) (count a-seq)))))


(defn old-book->new-book [book]
 (let [authorSet (set (book :authors))]
   (assoc book :authors authorSet)))

(defn has-author? [book author]
  (let [authors (book :authors)]
  (contains? authors author)))

(defn authors [books]
  (let [bookAuthors (fn [book] (set (:authors book)))]
    (set (apply concat (map bookAuthors books)))))


(defn all-author-names [books]
  (let [author-names (fn [books] (map :name (authors books)))]
    (set (author-names books))))

(defn author->string [author]
  (let [author-name (author :name)
        author-years (cond
                       (contains? author :death-year) (apply str [" (" (str (author :birth-year)) " - " (str (author :death-year)) ")"])
                       (contains? author :birth-year) (apply str [" (" (str (author :birth-year)) " - )"])
                       :else (str nil))]
    (apply str [author-name author-years])))

(defn authors->string [authors]
  (let [author-string (fn [author] (author->string author))]
  (apply str (interpose ", " (map author-string authors)))))

(defn book->string [book]
    (apply str [(:title book) ", written by " (authors->string (:authors book))]))

(defn books->string [books]
  (let [book-count (cond 
                     (== 0 (count books)) (str "No books.")
                     (== 1 (count books)) (str "1 book. ")
                     :else (apply str [(str (count books)) " books. "]))]
    (cond 
      (== 0 (count books)) book-count
      :else (apply str [book-count (apply str (interpose ". " (map book->string books)))]))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (let [filtered-authors (filter (fn [author] (= name (author :name))) authors)]
    (cond 
      (== 0 (count filtered-authors)) nil
      :else filtered-authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})

(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (let [has-living (fn [authors] (< 0 (count (living-authors authors))))]
    (has-living (book :authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
