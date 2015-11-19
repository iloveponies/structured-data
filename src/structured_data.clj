(ns structured-data)

(defn do-a-thing [x] (let [fname (fn [x] (Math/pow (+ x x) (+ x x)))] do (fname x)))

(defn spiff [v]
  (let [[x y z] v] do (+ x z)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] 
  (let [[x y z] v] do (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] 
  (let [[[x1 x2] [y1 y2]] rectangle]
  do (- y1 x1)))

(defn height [rectangle]
  (let [[[x1 x2] [y1 y2]] rectangle]
  do (- y2 x2)))

(defn square? [rectangle]
  (let [[[x1 x2] [y1 y2]] rectangle]
  do (if (== (- (- y2 x2) (- y1 x1)) 0) true false)))

(defn area [rectangle]
  (let [[[x1 x2] [y1 y2]] rectangle]
  do (* (- y2 x2) (- y1 x1))))

(defn contains-point? [rectangle point]
  (let [[[[x1 x2] [y1 y2]] [z1 z2]] [rectangle point]]
  do (if (and (<= x2 z2 y2) (<= x1 z1 y1)) true false) ))

(defn contains-rectangle? [outer inner]
  (let [[[[x1 x2] [y1 y2]] [[a1 a2] [b1 b2]]] [outer inner]]
  do (if (and (<= x2 a2 y2) (<= x2 b2 y2)
              (<= x1 a1 y1) (<= x1 b1 y1))
        true false) ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1) true false ))

(defn add-author [book new-author]
  (let [x (:authors book)]
    do (assoc book :authors (conj x new-author))))

(defn alive? [author]
  (if (:death-year author) false true ))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] 
  (let [munge (fn [x] (get x 1))]
    (map munge collection) ))

(defn titles [books]
  (let [getTitle (fn [book] (:title book))]
    (map getTitle books) ))

(defn monotonic? [a-seq] (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem] 
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem) ))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq))) true false))

(defn old-book->new-book [book]
 (assoc book :authors (set (:authors book))) )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [getAuthors (fn [book] (:authors book))]
   do (apply clojure.set/union (map getAuthors books)) ) )

(defn all-author-names [books]
  (let [getAuthors (fn [book] (map :name (:authors book)))]
   do (set (apply clojure.set/union (map getAuthors books))) ) )

(defn author->string [author]
  (str (:name author) (if (:birth-year author) 
    (str " (" (:birth-year author) " - " (:death-year author) ")") "")) )

(defn authors->string [authors]
  (apply str (interpose ", " (map (fn [x] (author->string x)) authors))))

(defn book->string [book] 
  (str (:title book) ", written by " 
    (apply str (interpose ", " (map (fn [x] (str (:name x) (if (:birth-year x) 
      (str " (" (:birth-year x) " - " (:death-year x) ")") ""))) (:authors book) )))))

(defn books->string [books]
  (if (empty? books) "No books." 
    (str (count books) " " (if (< 1 (count books)) "books. " "book. ") 
      (apply str (interpose ". " (map (fn [x] (book->string x)) books))) ".") ))

(defn books-by-author [author books]
  (let [findOut (fn [book] (if (contains? (:authors book) author) book false))]
    do (filter (fn [x] x) (map findOut books)) ))

(defn author-by-name [name authors]
  (let [findAuthor (fn [author] (if (= name (:name author)) author false))]
    do (first (filter (fn [x] x) (map findAuthor authors))) ) )

(defn living-authors [authors]
  (let [getAlive (fn [author] (if (:death-year author) false author))]
    do (filter (fn [x] x) (map getAlive authors)) ) )

(defn has-a-living-author? [book]
  (let [alive (fn [author] (if (:death-year author) false true))]
    do (contains? (set (map alive (:authors book))) true) ) )

(defn books-by-living-authors [books]
  (let [getBooks (fn [book] 
    (if (contains? (set (map :death-year (:authors book))) nil)
      book false) )] 
  do (set (filter (fn [x] x) (map getBooks books))) ))

; %________%
