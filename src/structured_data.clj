(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn sum-pairs [[x1 y1] [x2 y2]]
  [(+ x1 x2)
   (+ y1 y2)])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [[[x1, y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn width [rectangle]
  (let [[[x1, y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
    (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1, y1] [x2 y2]] rectangle]
  (let [[px, py] point]
  (and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[ix iy] [ix2 iy2]] inner]
    (and (contains-point? outer (point ix iy))
         (contains-point? outer (point ix2 iy2))
      )))

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;
;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
;                     :authors #{friedman, felleisen}})
;
;(def books [cities, wild-seed, embassytown, little-schemer])

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler", 
;              :birth-year 1972
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman", :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed" :authors [octavia]})
;(def embassytown {:title "Wild Seed" :authors [china]})
;(def little-schemer {:title "The Little Schemer" :authors [friedman felleisen]})
;(def books [cities wild-seed embassytown little-schemer])

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (count (get book :authors)) 1))

(defn add-author [book new-author]
  (let [ new-authors
    (conj (get book :authors) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

;(defn second-elements [collection]
;  (let [get-second (fn [seq] (get seq 1))]
;    (map get-second collection)))

(defn second-elements [collection]
  (let [get-second (fn [seq] (first (rest seq)))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [ a-set (set a-seq)]
    (not (= (count a-set) (count a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))) 

; contains? does not work on vectors, so
; we must convert the vector of authors to a set

(defn has-author? [book author]
    (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (let [book-authors (authors books)]
    (set (map :name book-authors))))

(defn author->string [author]
  (let [ name (:name author) 
         birth-year (:birth-year author)
         death-year (:death-year author) ]
    (if birth-year
      (str name " (" birth-year " - " death-year ")")
      (str name))
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)
        author-string (apply str (interpose ". " (map book->string books)))]
    (cond 
       (= book-count 0) "No books."
       (= book-count 1) (str "1 book. " author-string ".")
       :else (str book-count " books. " author-string "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

;(def authors #{china, felleisen, octavia, friedman})

(def jrrtolkien {:name "J. R. R. " :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})

(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn author-by-name [name authors]
  (let [author (filter (fn [author] (= name (:name author))) authors)]
  (first author)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
   (let [living-authors (living-authors (:authors book))]
      (not (empty? living-authors))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
