(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2 )]
    (if (and x y)
      (+ x y)
      nil)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (if (and x y)
      (+ x y)
      nil)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[bottom-left top-right]]
  (- (get top-right 0) (get bottom-left 0)))

(defn height [[bottom-left top-right]]
  (- (get top-right 1) (get bottom-left 1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [x y] point]
    (and (<= x0 x x1) (<= y0 y y1))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

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


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [a (or (get book :authors) [])
        b (conj a new-author)]
    (assoc book :authors b)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (clojure.string/join "" (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (=
   (count a-seq)
   (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (let [new-books (map old-book->new-book books)]
      (apply clojure.set/union (map :authors new-books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (:birth-year author)
    (if (:death-year author)
      (format "%s (%d - %d)" (:name author) (:birth-year author) (:death-year author))
      (format "%s (%d - )" (:name author) (:birth-year author)))
    (format "%s" (:name author))))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (format "%s, written by %s" (:title book) (authors->string (:authors book))))

(defn books->string [books]
  (let [b-n (count books)]
    (case b-n
      0 "No books."
      1 (format "1 book. %s." (book->string (get books 0)))
      ;; default
      (let [bs (clojure.string/join ". " (map book->string books))]
        (format "%d books. %s." b-n bs)))))



(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))


(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})


(defn has-a-living-author? [book]
  (let [l-as (living-authors (:authors book))]
    (not (empty? l-as))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
