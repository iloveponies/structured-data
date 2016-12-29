(ns structured-data)

; ********************** Authors/Books definitions **********************
; Placed here for reference. PLEASE DELETE WHEN DONE!!!
;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;
;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed", :authors [octavia]})
;(def embassytown {:title "Embassytown", :authors [china]})
;(def little-schemer {:title "The Little Schemer"
;                     :authors [friedman, felleisen]})
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
  "Returns true if the author "
  [author]
  (not (contains? author :death-year)))

(defn element-lengths
  "Retuns the length of every element in a collection"
  [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names
  "Retuns all authors given a collection of books"
  [books]
  ;(set (apply concat (map (map :name (:authors books)) books)))
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

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
