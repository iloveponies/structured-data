(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

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
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[p1 p2] point
        [[x1 y1][x2 y2]] rectangle]
    (if (and (>= x2 p1 x1) (>= y2 p2 y1)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1))
             (contains-point? outer (point x1 y2))
             (contains-point? outer (point x2 y1))
             (contains-point? outer (point x2 y2)))
      true false)))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (= (author-count book) 1) false true))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [f (fn [val] (get val 1))]
    (map f collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) 
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (fn [birth, death]
                (if death 
                  ;; True
                  (str " (" birth " - " death ")") 
                  ;; False
                  (if birth
                    ;; True
                    (str " (" birth " - )")
                    ;; False
                    "")))]
  (str (:name author) (years (:birth-year author) (:death-year author)))))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [func (fn [books] (apply str (interpose ". " (map book->string books))))
        num (fn [n] (if (== n 1) "1 book. " (str n " books. ")))]
    (if (empty? books) "No books." (str (num (count books)) (func books) ))))

(defn func [n] (if (== n 1) "1 book." (str n " books.")))
 
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

;;;;;=====================================================
;;;;;=====================================================
;;;;;=====================================================
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
;
;(def authors #{china, felleisen, octavia, friedman})
;
;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
;
;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})
;
;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
;
;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
;;;;;=====================================================
;;;;;=====================================================
;;;;;=====================================================

(defn author-by-name [name authors]
  (let [filtered (filter (fn [author] (= (:name author) name)) authors)]
    (if (> (count filtered) 0) (first filtered) nil)))

(defn living-authors [authors]
  (filter alive? authors))
  

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
