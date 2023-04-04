(ns structured-data)

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})
(def books [cities, wild-seed, embassytown, little-schemer])
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
(def authors #{china, felleisen, octavia, friedman})

(defn do-a-thing [x]
  (let [xsum (+ x x)]
  (Math/pow xsum xsum)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
  (+ first third)))

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

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 x2]) (contains-point? outer [y1 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [updated_authors (conj (:authors book) new-author)]
    (prn updated_authors)
   (assoc book :authors updated_authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec_el (fn [col] (get col 1))]
       (map sec_el collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author ))

(defn authors [books]
  (let [authors-set
        (set (map :authors books))]
  (apply clojure.set/union authors-set)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (if (contains? author :death-year) 
                (str " (" (get author :birth-year )
                     " - " (get author :death-year)
                     ")")
                (if (contains? author :birth-year) 
                  (str " (" (get author :birth-year) 
                       " - )" )))]
    (str (get author :name) years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookcount (count books)]
  (cond
    (> bookcount 1) (str 
                     bookcount 
                     " books. " 
                     (apply str (interpose 
                                 ". " 
                                 (map book->string books))))
    (== bookcount 1) (str 
                      "1 book. " 
                      (book->string (get books 0)))
    :else "No books."
    )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (identical? (:name a) name )) authors)))

(defn living-authors [authors]
  (filter (fn [a] (alive? a)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
