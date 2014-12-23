(ns structured-data)

(use 'clojure.set)

(defn do-a-thing [x]
  (let [dbl (+ x x)]
    (Math/pow dbl dbl)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 _ v2] v]
    (+ v0 v2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]] ;1st level [] = wrapping of params, 2nd level [] = destructuring vector
  (- x2 x1))

(defn height [[[_ y1] [_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (<= x1 px x2) (<= y1 py y2)))

(defn contains-rectangle? [outer [inner-p1 inner-p2]]
  (and (contains-point? outer inner-p1) (contains-point? outer inner-p2)))



(comment
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
  )



(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collections]
  (let [extractor (fn [col] (get col 1))]
    (map extractor collections)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (apply <= a-seq))

; compare left side to Scala's fn.tupled(tuple)
; (apply function [arg1 arg2 arg3 ...]) => (function arg1 arg2 arg3 ...)
; also remember PL2013 course where fn args are tuples,
; clojure also has tuple syntax of fn args: deffn name [arg1 arg2]

(defn stars [n]
  (apply str (repeat n "*"))) ; repeat gives a seq (like Scala tuple), we transform it to a multiple args call to str

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors-dedup (set (:authors book))]
    (assoc book :authors authors-dedup)))



(comment
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

  (def authors1 #{china, felleisen, octavia, friedman})

  )



(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let
    [birth (:birth-year author)
     death (:death-year author)
     years-str (cond
                 (and death) (str " (" birth " - " death ")")
                 (and birth) (str " (" birth " - )")
                 :else "")]
    (str (:name author) years-str)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [books-amount (count books)
        amount-str (str books-amount " book")
        books-list-str (str ". " (apply str (interpose ". " (map book->string books))) ".")]
    (cond
      (< books-amount 1) "No books."
      (= books-amount 1) (str amount-str books-list-str)
      :else (str amount-str "s" books-list-str))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))



(comment
  (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
  (def christopher {:name "Christopher Tolkien" :birth-year 1924})
  (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

  (def silmarillion {:title "Silmarillion"
                     :authors #{jrrtolkien, christopher, kay}})

  (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
  (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

  (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

  )


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
