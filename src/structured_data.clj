(ns structured-data)
;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})
;; 
;; (def cities {:title "The City and the City" :authors [china]})
;; (def wild-seed {:title "Wild Seed", :authors [octavia]})
;; (def embassytown {:title "Embassytown", :authors [china]})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})
;; (def books [cities, wild-seed, embassytown, little-schemer])
;; 
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})
;; 
;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})
;; 
;; (def cities {:title "The City and the City" :authors #{china}})
;; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;; (def embassytown {:title "Embassytown", :authors #{china}})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})
;; 
;; (def books [cities, wild-seed, embassytown, little-schemer])
;; 
;; (def authors #{china, felleisen, octavia, friedman})
;; 
;; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
;; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})
;; 
;; (def silmarillion {:title "Silmarillion"
;;                    :authors #{jrrtolkien, christopher, kay}})
;; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
;; 
;; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn do-a-thing [x]
  (let [sum (+ x x)]
       (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[one two three] v]
    (+ one three)))

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
  (if (= (width rectangle) (height rectangle) )
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[left-bottom right-top] inner]
    (if (and
         (contains-point? outer left-bottom)
         (contains-point? outer right-top))
      true
      false)))

(defn title-length [book]
  (count (get book :title)))


(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1)
    true
    false
))

(defn add-author [book new-author]
  ;; (let [authors (get book :authors)]
    ;; (let [combined-authors (conj authors new-author)]
    ;; (assoc book :authors combined-authors))   
    ;; )
  (assoc book :authors (conj (get book :authors) new-author))
  )

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element
        (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq)))
    true
    false))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [result-name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if-not (nil? (:birth-year author))
      (str result-name years)
      result-name
      )
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (> (count books) 1)
    (str (count books) " books. "
         (apply str (interpose ". " (map book->string books))) ".")
    (if (= (count books) 1)
      (str "1 book. " (apply str (map book->string books)) ".")
      "No books."))
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors)  )

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
