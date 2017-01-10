(ns structured-data)

; ********************** Authors/Books definitions **********************
; Placed here for reference. PLEASE DELETE WHEN DONE!!!
;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
;
;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title   "The Little Schemer"
;                     :authors #{friedman, felleisen}})
;(def books [cities, wild-seed, embassytown, little-schemer])
;(def authors-names #{china, felleisen, octavia, friedman})
;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})
;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
; Placed here for reference. PLEASE DELETE WHEN DONE!!!
; ********************** Authors/Books definitions **********************
(defn do-a-thing
  "Use let to accopmplish the example given"
  [x]
  (let [n (+ x x)]
    (Math/pow n n)))

(defn spiff [v]
  "Returns the sum of the 1st and 3rd elements of a vector"
  (if (>= (count v) 3)
    (+ (get v 0) (get v 2))
    nil))

(defn cutify [v]
  "Return a new vector with <3 appended to the end of the original vector"
  (let [symbol "<3"]
    (conj v symbol)))

(defn spiff-destructuring
  "Rewrite spiff function to include destructuring"
  [v]
  (if (>= (count v) 3)
    (let [[a t b] v]
      (+ a b))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width
  "Return the width of a given rectangle"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height
  "Return the height of a given rectangle"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square?
  "Return true if given rectangle is square, else false"
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1))
      true
      false)))

(defn area
  "Return the area of a given rectangle"
  [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* h w)))

(defn contains-point?
  "Return true if a given rectangle has a point within, else false"
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (if (and (<= x1 p1 x2) (<= y1 p2 y2))
      true
      false)))

(defn contains-rectangle?
  "Return true if a given outer rectangle contains another inner rectangle within, else false"
  [outer inner]
  (let [rectangle outer
        [p3 p4] inner]
    (if (and (contains-point? rectangle p3) (contains-point? rectangle p4))
      true
      false))
  )

(defn title-length
  "Return the length of the given book's title"
  [book]
  (count (:title book)))

(defn author-count
  "Return the number of authors for a given book"
  [book]
  (count (:authors book)))

(defn multiple-authors?
  "Returns true if the given books has multiple authors, else false"
  [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author
  "Add a new author to a given book"
  [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive?
  "Returns true if the given author is alive (has no death-year)"
  [author]
  (not (contains? author :death-year)))

(defn element-lengths
  "Retuns the length of every element in a collection"
  [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements
  "Returns a sequence of second elements given a collection"
  [collection]
  (let [extract-second (fn [x] (second x))]
    (map extract-second collection)))

(defn titles
  "Returns titles given a collection of books"
  [books]
  (let [extract-title (fn [x] (:title x))]
    (map extract-title books)))

(defn monotonic?
  "Returns true if a sequence is monotonic, false otherwise"
  [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars
  "Returns a sequence of n stars"
  [n]
  (apply str (repeat n "*")))

(defn toggle
  "Removes elem from a-set if it exists, else adds it to a-set"
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates?
  "Returns true if collection returns duplicates, false otherwise"
  [a-seq]
  (if (> (count a-seq) (count (set a-seq)))
    true
    false))

(defn old-book->new-book
  "Converts the author format from vector to set"
  [book]
  (assoc book :authors (set (:authors book))))

(defn has-author?
  "Returns true if given book has author, else false"
  [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors
  "Returns a set of authors given books"
  [books]
  (let [author (fn [book] (:authors book))]
    (apply clojure.set/union (map author books))))

(defn all-author-names
  "Retuns all authors given a collection of books"
  [books]
  (let [author (fn [book] (:authors book))]
    (set (map :name (apply clojure.set/union (map author books))))))

(defn author->string
  "Return the string representation of an author"
  [author]
  ;Alternate solution
  ;(let [name (:name author)
  ;      birth (:birth-year author)
  ;      death (:death-year author)]
  ;  (cond
  ;    (and birth death) (str name " (" birth " - " death ")")
  ;    (and birth (not death)) (str name " (" birth " - )")
  ;    :else (str name)))
  (if (:birth-year author)
    (if (:death-year author)
      (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
      (str (:name author) " (" (:birth-year author) " - )"))
    (str (:name author))))

(defn authors->string
  "Return the string representation of one or more authors"
  [authors]
  (let [stringify (fn [author] (:name author)
                    (if (:birth-year author)
                      (if (:death-year author)
                        (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
                        (str (:name author) " (" (:birth-year author) " - )"))
                      (str (:name author))))]
    (apply str (interpose ", " (map stringify authors)))))


(defn book->string
  "Return the string representation of a book"
  [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string
  "Returns a string representation of 0 or more books"
  [books]
  (let [book-string (fn [book] (str (book->string book) "."))
        num-books   (count books)]
    (cond
      (= num-books 0) (str "No books.")
      (= num-books 1) (str num-books " book. " (apply str (interpose " " (map book-string books))))
      :else (str num-books " books. " (apply str (interpose " " (map book-string books)))))
    )
)

(defn books-by-author
  "Return the book(s) by a given author"
  [author books]
  (let [author-exists? (fn [book]
                         (if (has-author? book author) book))]
    (into [] (filter #(if (nil? %) false true) (map author-exists? books)))))

(defn author-by-name
  "Returns the author by the given name from a list of authors, nil otherwise"
  [target-name authors]
  (first (filter (fn [{:keys [name]}] (= name target-name)) authors)))

(defn living-authors
  "Returns living authors from a given collection of authors"
  [authors]
  (filter alive? authors))

(defn has-a-living-author?
  "Returns true if given book has living authors"
  [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors
  "Returns books that contain living authors"
  [books]
  (filter has-a-living-author? books))


; %________%
()