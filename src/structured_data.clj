(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
       (Math/pow xx xx)))

(defn spiff [v]
  (cond
    (< (count v) 3) nil
    :else (+ (first v) (get v 2))))

(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[f l] [(first v) (get v 2)]]
    (+ f l)))

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
  (if (= (height rectangle) (width rectangle)) true false))


(defn area [rectangle]
  (* (height rectangle) (width rectangle)))



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1)
             (contains-point? outer p2)) true false) ))



(comment
;;for testing in REPL

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

  (def books [cities, wild-seed, embassytown, little-schemer])
  (def authors #{china, felleisen, octavia, friedman})

)


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (set (apply concat (map author-names books))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn mungefy [a-seq]
  (let [munge (fn [x] (+ x 42))]
    (map munge a-seq)))
(mungefy [1 2 3 4])


(defn second-elements [collection]
  (let [scd (fn [x] (get x 1))]
    (map scd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (let [authers (:authors book)]
    (assoc book :authors (set authers))))


(comment
  ;;For testing in REPL

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
  (def authors #{china, felleisen, octavia, friedman})

)

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [book-authors
        (fn [book] (:authors book))]
    (apply clojure.set/union (map book-authors books) )))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if birth
      (str name " (" birth " - " death ")")
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)
        bks (apply str (interpose ". " (map book->string books)))]
    (cond
      (= n 0) (str "No books.")
      (= n 1) (str n " book. " bks ".")
      :else (str n " books. " bks "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (if (= name (:name x)) true false)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(comment
  ;; For testing in REPL

  (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
  (def christopher {:name "Christopher Tolkien" :birth-year 1924})
  (def kay {:name "Guy Gavriel Kay" :birth-year 1954})
  (def silmarillion {:title "Silmarillion" :authors #{jrrtolkien, christopher, kay}})
  (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
  (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
  (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

)


(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
