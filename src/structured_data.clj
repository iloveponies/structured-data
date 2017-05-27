(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [eka (get v 0)
        toka (get v 2)]
     (+ eka toka)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[eka tyhja toka] v]
    (+ eka toka)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
          (contains-point? outer top-right))))

(comment

  (def china {:name "China Miéville" :birth-year 1972})
  (def octavia {:name "Octavia E. Butler"
              :birth-year 1947
             :death-year 2006})
  (def friedman {:name "Daniel Friedman" :birth-year 1944})
  (def felleisen {:name "Matthias Felleisen"})

  (def allauthors [china, octavia, friedman, felleisen])

  (def cities {:title "The City and the City" :authors #{china}})
  (def wild-seed {:title "Wild Seed", :authors #{octavia}})
  (def embassytown {:title "Embassytown", :authors #{china}})
  (def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

  (def books [cities, wild-seed, embassytown, little-schemer])

)

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
    (set (apply concat (map author-names books))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
  (let [auth (:authors book)]
    (assoc book :authors (conj auth new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))



(defn second-elements [collection]
  (let [toka (fn [v] (get v 1))]
    (map toka collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [nimi (:name author)
        years (if (contains? author :birth-year)
                (if (not (alive? author))
                    (str " (" (:birth-year author) " - " (:death-year author) ")")
                    (str " (" (:birth-year author) " - )"))
              "")]
    (str nimi years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (let [nimi (:title book)
        bookauthors (authors->string (:authors book))]
    (str nimi ", written by " bookauthors)))

(defn books->string [books] ; ei toimi
   (let [books-as-string (apply str (interpose ". " (map book->string books)))
         nbooks (str (count books))
         numstring (if (== (count books) 1) (str nbooks " book") (str nbooks " books"))]
     (if (== (count books) 0) "No books." (str numstring ". " books-as-string "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
