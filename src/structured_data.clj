(ns structured-data)

(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn sum-pairs [first-pair second-pair]
  [(+ (first first-pair) (first second-pair))
   (+ (second first-pair) (second second-pair))])

(defn sum-pairs-pattern [[xone yone] [xtwo ytwo]]
  [(+ xone xtwo) (+ yone ytwo)])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xone _] [xtwo _]] rectangle]
    (- xtwo xone)))

(defn height [rectangle]
  (let [[[_ yone] [_ ytwo]] rectangle]
    (- ytwo yone)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[xone yone] [xtwo ytwo]] rectangle
        [x y] point]
    (and (<= xone x xtwo) (<= yone y ytwo))))

(defn contains-rectangle? [outer inner]
  (let [[plower pupper] inner]
    (and (contains-point? outer plower) (contains-point? outer pupper))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (count (:authors book)) 2))

(defn add-author [book new-author]
  (let [final-authors (conj (:authors book) new-author)]
    (assoc book :authors final-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors #{china}})
;; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;; (def embassytown {:title "Embassytown", :authors #{china}})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})

;; (def books [cities, wild-seed, embassytown, little-schemer])

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union
              (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
       (if (nil? (:birth-year author))
         ""
         (str " (" (str (:birth-year author))
              " - " (str (:death-year author)) ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (let [final-string (str (apply str (interpose ". " (map book->string books))) ".")
        num-book (count books)]
    (cond
      (= 0 num-book) "No books."
      (= 1 num-book) (str "1 book. " (str final-string))
      :else (str (str num-book) " books. " (str final-string)))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [lst (filter (fn [x] (= name (:name x))) authors)]
    (cond
      (empty? lst) nil
      :else (first lst))))

;;(def authors #{china, felleisen, octavia, friedman})

(defn living-authors [authors]
  (filter (fn [x] (nil? (:death-year x))) authors))

;; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
;; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;; (def silmarillion {:title "Silmarillion"
;;                    :authors #{jrrtolkien, christopher, kay}})
;; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
