(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn sum-pairs1 [first-pair second-pair]
  [(+ (first first-pair)  (first  second-pair))
   (+ (second first-pair) (second second-pair))])

(defn sum-pairs2 [[x1 y1][x2 y2]]
  [(+ x1 x2)(+ y1 y2)])

(defn sum-pairs3 [first-pair second-pair]
  (let [[x1 y1] first-pair
        [x2 y2] second-pair]
    [(+ x1 x2) (+ y1 y2)]))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

; (0,0)
;     (1,1)           (5,1)
;         (2,3)
;
;
;                     (5,5)
;
;

(defn width [rectangle]
  (Math/abs (- (first (first rectangle)) (first (second rectangle)))))

(defn width0 [rectangle]
  (let [[bl tr] rectangle]
    (let [[x1 y1] bl
          [x2 y2] tr]
      (Math/abs (- x1 x2)))))

(defn width1 [[bl tr]]
  (Math/abs (- (first bl) (first tr))))

(defn width2 [[bl tr]]
  (let [[x1 y1] bl
          [x2 y2] tr]
      (Math/abs (- x1 x2))))

(defn width3 [[[x1 y1] [x2 y2]]]
  (Math/abs (- x1 x2)))

(defn height [rectangle]
  (Math/abs (- (second (first rectangle)) (second (second rectangle)))))

(defn height1 [[[x1 y1] [x2 y2]]]
  (Math/abs (- y1 y2)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn square0? [[[x1 y1] [x2 y2]]]
  (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (and
    (<= (first (first rectangle)) (first point))
    (<= (first point) (first (second rectangle)))
    (<= (second (first rectangle)) (second point))
    (<= (second point) (second (second rectangle)))
  ))

(defn contains-rectangle? [outer inner]
  (and
    (contains-point? outer (first inner))
    (contains-point? outer (second inner))))

(defn title-length [book]
 (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (count (get book :authors)) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [kunge (fn [x] (get x 1))]
    (map kunge collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (< (first a-seq) (last a-seq)) (apply <= a-seq) (apply <= (reverse a-seq)))
)

(defn stars [n]
  (let [starry (repeat n "*")]
    (apply str starry)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))



;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
;                     :authors #{friedman, felleisen}})

;(def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
  (contains? (get book :authors) author))

;(defn authors1 [books]
 ; (let [author
  ;      (fn [book] (map :name (:authors book)))]
   ; (set (apply clojure.set/union (map author books)))))

(defn authors [books]
  (let [author (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (:name author)))

(defn authors->string [authors]
   (apply str (interpose ", " (clojure.set/union (map author->string authors)))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let [x (count books)]
    (if (= x 0)
      "No books."
      (if (> x 1)
        (str x " books. " (apply str (interpose ". " (clojure.set/union (map book->string books)))) ".")
        (str x " book. " (apply str (map book->string books)) ".")
        )
      )))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


;(def authors #{china, felleisen, octavia, friedman})

(defn has-name? [author name]
  (= (:name author) name))

(defn author-by-name [name authors]
  (first (filter (fn [x] (has-name? x name)) authors)))

;(defn author-by-name1 [name authors]  (map :name authors))

;(author-by-name "Daniel Friedman" authors)
;(author-by-name "Octavia E. Butler" authors)                ;=> octavia
;(author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
;(author-by-name "China Miéville" authors)                   ;=> china
;(author-by-name "Goerge R. R. Martin" authors)


(defn living-authors [authors]
  (filter alive? authors))


;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;(def silmarillion {:title "Silmarillion"
;                  :authors #{jrrtolkien, christopher, kay}})
;
;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (if (empty? (filter alive? (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
