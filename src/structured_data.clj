(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (if (and a b)
      (+ a b)
      nil)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (if (== (height rectangle) (width rectangle))
  true
  false))



(defn area [rectangle]
  (* (height rectangle) (width rectangle)))





(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2))
      true
      false)))




(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1))
             (contains-point? outer (point x2 y2)))
      true
      false)))




(defn title-length [book]
  (count (:title book)))




(defn author-count [book]
  (count (:authors book)))




(defn multiple-authors? [book]
  (if (== 1 (author-count book))
    false
    true))



(defn add-author [book new-author]
  (let [a (:authors book)]
    (assoc book :authors (conj a new-author))))




(defn alive? [author]
  (if (contains? author :death-year)
  false
  true))



(defn element-lengths [collection]
  (map count collection))




(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))


(defn titles [books]
  (map :title books))

;        :D



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
  (let [a-set (set a-seq)]
    (if (== (count a-seq) (count a-set))
      false
      true)))




(defn old-book->new-book [book]
   (assoc book :authors (set (:authors book))))




;; (old-book->new-book {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})
;; ;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
;; (old-book->new-book {:title "Wild Seed", :authors [octavia]})
;; ;=> {:title "Wild Seed", :authors #{octavia}}


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


(defn has-author? [book author]
  (contains? (:authors book) author))





(defn authors [books]
  (apply clojure.set/union (map :authors books)))










(defn all-author-names [books]
  (set(map :name (authors books))))




(defn author->string [author]
  (let [name (:name author)
        years (cond (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
                    (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                    :else "")]
    (str name years )))



(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors))))



(defn book->string [book]
  (let [title (:title book)]
    (str title ", written by " (authors->string (:authors book)))))






(defn books->string [books]
  (let [n (count books)
        amount (cond
                     (== n 1) "1 book. "
                     :else (str n " books. "))]

    (if (== n 0)
      "No books."
      (str (apply str amount (interpose ". "(map book->string books))) "."))
    ))


(defn books-by-author [author books]
  (let [hasauthor (fn [x] (has-author? x author))]
    (filter hasauthor books)))



;; (def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (let [namematch (fn [auth] (= name (:name auth)))]

    (first (filter namematch authors))))


;; (author-by-name "Octavia E. Butler" authors)                ;=> octavia
;; (author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
;; (author-by-name "China Miéville" authors)                   ;=> china
;; (author-by-name "Goerge R. R. Martin" authors)              ;=> nil


;; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
;; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;; (def silmarillion {:title "Silmarillion"
;;                    :authors #{jrrtolkien, christopher, kay}})

;; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn living-authors [authors]
  (filter alive? authors))



(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))




(defn books-by-living-authors [books]
  (filter has-a-living-author? books))



; %________%


