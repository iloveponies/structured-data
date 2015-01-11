(ns structured-data)

(defn do-a-thing [x] (let [sum (+ x x)] (Math/pow sum sum)))


(defn spiff [x] (+ (get x 2) (get x 0)))

(defn cutify [x] (conj x "<3"))


(defn spiff-destructuring [[x1 x2 x3]] (+ x1 x3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [[[x1 y1] [x2 y2]]] (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]] (- y2 y1))

(defn abs [x] (if (< x 0) (* x -1) x))

(defn square? [[rec1 rec2]] (let [[x1 y1] rec1 [x2 y2] rec2] (= (abs(- x1 x2)) (abs(- y2 y1)))))



(defn area [[rec1 rec2]] (let [[x1 y1] rec1 [x2 y2] rec2] (* (abs(- y2 y1)) (abs(- x2 x1)))))


(defn contains-point? [ [rec1 rec2] point] (let [ [x1 y1] rec1 [x2 y2] rec2 [xp yp] point] (if (and  (<= x1 xp x2) (<= y1 yp y2)) true false)))

(defn contains-rectangle? [rec1 rec2] (let [ [pair1 pair2] rec2 [x1 y1] pair1 [x2 y2] pair2] (if (and (contains-point? rec1 [x1 y1]) (contains-point? rec1 [x2 y2])) true false)))

;Only for the test in REPL
;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
 ;             :birth-year 1947
  ;            :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed", :authors [octavia]})
;(def embassytown {:title "Embassytown", :authors [china]})
;(def little-schemer {:title "The Little Schemer"
 ;                    :authors [friedman, felleisen]})


(defn title-length [book] (count (get book :title)))

(defn author-count [book] (count (get book :authors)))


(defn multiple-authors? [book] (if (> (author-count book) 1)  true false))


(defn add-author [book author] (let [ authors (get book :authors)] (assoc book :authors (conj (get book :authors) author))))


(defn alive? [author] (not(contains? author :death-year)))


(defn element-lengths [col] (map count col))



(defn second-elements [cols] (let [second-fun (fn [col1] (get col1 1))] (map second-fun cols)))


(defn titles [book] ( map :title book))

(defn monotonic? [seq1] (if (apply <= seq1) true (if (apply >= seq1) true false)))


(defn stars [n] (apply str (repeat n "*")))


(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn contains-duplicates? [col] (if (> (count col) (count (set col))) true false))

(defn old-book->new-book [col] (let [auth (set (:authors col))] (assoc col :authors auth)))


(defn has-author? [book author] (contains? (:authors book) author))


(defn authors [books] (apply clojure.set/union ( map :authors  books)))


(defn all-author-names [books] (set(map :name (authors books))))

(defn author->string [author] (let [name-author (:name author) birth-year (:birth-year author) death-year (:death-year author)] (if (or (not (nil? death-year)) (not (nil? birth-year))) (str name-author " (" birth-year " - " death-year ")") (str name-author)))) 

(defn authors->string [authors] (apply str (interpose ", "  (map author->string authors))))


(defn book->string [book] (str (:title book) ", written by " (authors->string (:authors book))))

;(defn books->string [books] (if (< (count books) 1) (str "No books.") (str (count books) "book" (if (> (count books) 1) (str "s ") (str " ")) (apply str (interpose ". " (map book->string books))))))
;(defn books->string [books] (if (< (count books) 1) (str "No books.") (str (count books) " book" (if (> (count books) 1) (str ".s ") (str ". ")) (apply str (interpose ". " (map book->string books))))))
(defn books->string [books] (if (< (count books) 1) (str "No books.") (str (count books) " book" (if (> (count books) 1) (str "s. ") (str ". ")) (apply str (interpose ". " (map book->string books))) (str "."))))


(defn books-by-author [author books] (filter (fn [book] ( has-author? book author)) books))


;(defn author-by-name [name authors] (filter (fn [author] (= (:name author) name)) authors))
(defn author-by-name [name authors] (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors] (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book] (if (> (count (living-authors (:authors book))) 0) true false)) 


;(defn books-by-living-authors [books] (filter (fn [book] (living-authors (:authors book))) books))
(defn books-by-living-authors [books] (filter (fn [book] (not (empty? (living-authors (:authors book))))) books))


; %________%
