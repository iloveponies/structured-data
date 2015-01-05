(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
  (Math/pow x+x x+x)))

(defn spiff [v]
  "Returns the sum of the first and third elements of a vector"
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  "Adds <3 to the end of a vector"
  (conj v "<3"))

(defn spiff-destructuring [v]
  "Returns the sum of the first and third elements of a vector-DESTRUCTURE STYLE!"
  (let [[x y z] v]
    (+ x z)))

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
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  "Returns true if rectangle contains point and otherwise false"
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
  (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  "Returns true if the rectangle inner is inside the rectangle outer and otherwise false"
  (let [[ix iy] inner]
    (if (and (contains-point? outer ix)
           (contains-point? outer iy)) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (>= (author-count book) 2) true false))

(defn add-author [book new-author]
  "Takes a book and an author and adds author to books authors"
  (let [authors (get book :authors)
        updated-authors (conj authors new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  "Returns the length of every item in the collection"
  (map count collection))

(defn second-elements [collection]
  "Takes a vector of vectors and returns a sequence of the second elements"
  (let [get-second (fn [vector] (get vector 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  "A sequence is monotonic if is either inceasing or decreasing."
  "In a decreasing sequence every element is at most as large as the previous one. "
  "In an increasing sequence every member is at least as large as the previous one."
  (let [increasing? (fn [a-seq] (apply <= a-seq))
        decreasing? (fn [a-seq] (apply >= a-seq))]
  (if (or (increasing? a-seq) (decreasing? a-seq)) true false)))

(defn stars [n]
  "Returns a string with n asterisks \\*"
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  "Removes elem from a-set if a-set contains elem, otherwise adds it to the set"
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  "Takes a book with authors as a vector and returns the same book with authors in a set"
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  "Returns the authors of every book in books as a set."
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  "(author->string felleisen) ;=> 'Matthias Felleisen'"
  "(author->string friedman)  ;=> 'Daniel Friedman (1944 - )'"
  "(author->string octavia)   ;=> 'Octavia E. Butler (1947 - 2006)'"
  (let [name (:name author)
        years [(:birth-year author) (:death-year author)]]
    (cond
     (and (= (get years 0) nil) (= (get years 1) nil)) (str name)
     (= (get years 1) nil) (str name " " \( (get years 0) " - " \))
     :else (str name " " \( (get years 0) " - " (get years 1) \)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  ;= (book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
  ;= (book->string little-schemer) ;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [quantity (count books)]
    (cond
     (= quantity 0) "No books."
     (= quantity 1) (str (count books) " book. " (apply str (map book->string books)) ".")
     :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
