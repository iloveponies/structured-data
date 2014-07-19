(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [xp yp]]
  (if (and(<= x1 xp x2)
          (<= y1 yp y2))
    true
    false))

(defn contains-rectangle? [outer inner]
  (let [[inner1 inner2] inner]
    (if (and (contains-point? outer inner1)
             (contains-point? outer inner2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-elm (fn [collection] (get collection 1))]
    (map second-elm collection)))

(defn second-elements2 [collection]
  (let [second-elm (fn [[a b]] b)]
    (map second-elm collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        year-str (if (or (:birth-year author)
                         (:death-year author))
                   (str " (" (:birth-year author) " - " (:death-year author ) \))
                   "")]
    (str name year-str)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [num-books (count books)
        books-str (if (> num-books 0)
                    (str (apply str (interpose ". " (map book->string books))) ".")
                    "")
        num-books-str (cond
                       (= num-books 0) "No books."
                       (= num-books 1) "1 book. "
                       :else (str num-books " books. "))]
    (str num-books-str books-str)))

(defn books-by-author [author books]
  (filter (fn [book]
            (has-author? book author))
          books))

(defn author-by-name [name authors]
  (let [filtered (filter (fn [author]
                           (= (:name author) name))
                         authors)]
    (first filtered)))

(defn author-by-name2 [name authors]
  (cond
   (= (first authors) nil) nil
   (= (:name (first authors)) name) (first authors)
   :else (author-by-name name (rest authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
(comment (def china {:name "China Miéville", :birth-year 1972})
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

         (old-book->new-book {:title "The Little Schemer"
                              :authors [friedman, felleisen]}))

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
  (def authors #{china, felleisen, octavia, friedman})

  (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
  (def christopher {:name "Christopher Tolkien" :birth-year 1924})
  (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

  (def silmarillion {:title "Silmarillion"
                     :authors #{jrrtolkien, christopher, kay}})

  (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
  (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

  (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

  (books-by-living-authors (concat books [deus-irae, silmarillion])))
