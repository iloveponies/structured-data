(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [f (v 0)
        t (v 2)]
    (+ f t)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f s t] v]
    (+ f t)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2)
         (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[ll ur] inner]
    (and (contains-point? outer ll)
         (contains-point? outer ur))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [v] (get v 1))]
    (map snd collection)))

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
  (let [unique (count (set a-seq))
        total (count a-seq)]
    (not (= unique total))))

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
        birth-year (:birth-year author)
        death-year (:death-year author)
        year (if birth-year
               (str " (" birth-year " - " death-year ")")
               "")]
    (str name year)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [book-count (count books)
        book-summary (cond
                       (= book-count 0) "No books."
                       (= book-count 1) "1 book."
                       :else (str book-count " books."))
        book-details (map #(str (book->string %) ".") books)
        book-pieces (cons book-summary book-details)]
    (str (apply str (interpose " " book-pieces)))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [alive (living-authors (:authors book))]
    (not (empty? alive))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

(defn hypotenuse
  [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

(defn sum-pairs [[x1 y1] [x2 y2]]
  [(+ x1 x2)
   (+ y1 y2)])

(defn mungefy [a-seq]
  (let [munge (fn [x] (+ x 42))]
    (map munge a-seq)))

(comment
; Authors
(def china {:name "China Miéville" :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})
(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

; Books
(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed" :authors #{octavia}})
(def embassytown {:title "Embassytown" :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman felleisen}})
(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien christopher kay}})
(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(def books [cities, wild-seed, embassytown little-schemer])
(def authors #{china felleisen octavia friedman})
)
(defn all-authors-names [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

