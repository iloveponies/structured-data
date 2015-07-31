
(ns structured-data)
;first (require 'structured-data); then (use 'structured-data)
;then (require 'clojure.set) then (use 'clojure.set) then (union mySet1 mySet2)
;user=> (require 'clojure.string)
;user=> (clojure.string/split "name,address,city,state,zip,email,phone" #",")
(require 'clojure.set)
(use 'clojure.set)

(def myFuncAddOne(fn[x](+ x 1)))
(println (myFuncAddOne 2))

(def china
  {:name "China Miéville",
   :birth-year 1972})
(def octavia
  {:name "Octavia E. Butler"
  :birth-year 1947
  :death-year 2006})
(def friedman
  {:name "Daniel Friedman"
  :birth-year 1944})
(def felleisen
  {:name "Matthias Felleisen"})
(def dick
  {:name "Philip K. Dick",
  :birth-year 1928,
  :death-year 1982})
(def zelazny
  {:name "Roger Zelazny",
   :birth-year 1937,
   :death-year 1995})

(def cities
  {:title "The City and the City"
   :authors [china]})
(def wild-seed
  {:title "Wild Seed",
   :authors [octavia]})
(def embassytown
  {:title "Embassytown",
   :authors [china]})
(def little-schemer
  {:title "The Little Schemer"
   :authors [friedman, felleisen]})
(def jrrtolkien
  {:name "J. R. R. Tolkien"
   :birth-year 1892
   :death-year 1973})
(def christopher
  {:name "Christopher Tolkien"
   :birth-year 1924})
(def kay
  {:name "Guy Gavriel Kay"
   :birth-year 1954})
(def silmarillion
  {:title "Silmarillion"
   :authors #{jrrtolkien, christopher, kay}})
(def deus-irae
  {:title "Deus Irae",
   :authors #{dick, zelazny}})

(def authors #{china, felleisen, octavia, friedman, dick, zelazny})

(def sanakirja{"hevonen" "horse", "poni" "pony"})

(def books [cities, wild-seed, embassytown, little-schemer, deus-irae, jrrtolkien, christopher, kay, silmarillion])

(defn do-a-thing [x] (let [a (+ x x)] (Math/pow a a)))

(defn spiff-destructuring [v]
  [(get v 0) (get v 2)])

(defn spiff [v]
  (if (>= (count v) 3)
    (let [x (spiff-destructuring v)] (+ (get x 0) (get x 1)))
    :?))

(defn cutify [v]
  (conj v "<3"))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  ;rectangle == [[hor1 ver1] [hor2 ver2]] two dimensional array
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  :-)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let
    [[[x1 y1] [x2 y2]] rectangle, [pointX pointY] point]
    (and (>= pointX x1) (<= pointX x2) (>= pointY y1) (<= pointY y2))))

(defn contains-rectangle? [outer inner]
  (let
    [[[inx1 iny1] [inx2 iny2]] inner, [[outx1 outy1][outx2 outy2]] outer]
    (and (>= inx1 outx1) (<= inx2 outx2) (>= iny1 outy1) (<= iny2 outy2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (conj (:authors book) new-author))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection)) ;anonymous innerfunction

(defn second-elements [collection]
  (let [getSecond (fn[innerArray] (innerArray 1))]
    (map getSecond collection)))

(defn titles [books]
  (map :title books))

(defn stars [n]
  (repeat n "*"))

;Number is monotinic if (<= a b c ... ) or (>= a b c ... )
(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn toggle [a-set elem]
  (if(contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(==(count a-seq)(count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

;(defn authors [books]
;  (apply union(map :authors (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map :name (authors books))))
;(defn all-author-names [books]
;  (let [author-names
;        (fn [book] (map :name (:authors book)))]
;    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (str
    (get author :name)
    (if(contains? author :birth-year)
      (str" ("(get author :birth-year)" - "(get author :death-year)")")
      "")
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (== 0 (count books))
    "No books."
    (if(== (count books) 1)
      (str "1 book. " (book->string (get books 0)) ".")
      (str (count books) " books. " (apply str (interpose ", " (map book->string books)))".")
    )
  )
)

(defn abcd[x]
  (apply str
    (if (== 1 x)
      ["abc" "123"]
      (if(== 3 x)
        ["111" "222"]
        ["aaa" "bbb"]
      )
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (contains? (set (get book :authors)) author)) books))
;(println (books-by-author china books))
;(println (books-by-author octavia books))

(defn author-by-name [name authors]
  (first (filter (fn[x](= (str(get x :name)) name)) authors))
)

;(println (author-by-name "Octavia E. Butler" authors))
;(println (author-by-name "Octavia E. Butler" #{felleisen, friedman}))
;(println (author-by-name "China Miéville" authors))
;(println (author-by-name "Goerge R. R. Martin" authors))

(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

