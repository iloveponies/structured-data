(ns structured-data)


;; (Math/pow (+ x x) (+ x x))

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


;; that takes a vector and returns the sum of the first and third elements

(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))



;; Rewrite our earlier function spiff by destructuring its parameter

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))



(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))


;; returns true if rectangle is a square and otherwise false

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))



;; returns the area of the given rectangle.

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


;; returns true if rectangle contains point and otherwise false

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))



;;  returns true if the rectangle inner is inside the rectangle outer and otherwise false

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))


;; counts the length of the book’s title.

(defn title-length [book]
  (count (:title book)))


;;  returns the amount of authors that book has

(defn author-count [book]
  (count (book :authors)))


;; returns true if book has multiple authors, otherwise false

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))



(defn add-author [book new-author]
  (let [{authors :authors} book
        new_authors (conj authors new-author)]
    (assoc book :authors new_authors)
    ))



;;  takes an author map and returns true if the author is alive, otherwise false.

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))



;; returns the lengths of every item in collection

(defn element-lengths [collection]
  (map count collection))


;;  takes a vector of vectors and returns a sequence of the second elements.

(defn second-elements [collection]
  (let [sec-el (fn [x] (get x 1))]
    (map sec-el collection)))


;; takes a collection of books and returns their titles

(defn titles [books]
  (map :title books))


;; returns true if a-seq is monotonic and otherwise false.

(defn monotonic? [a-seq]
  (let [[x y] a-seq]
    (if (>= x y)
      (apply >= a-seq)
      (apply < a-seq))))


;; returns a string with n asterisks \*

(defn stars [n]
  (apply str (repeat n "*")))


;; removes elem from a-set if a-set contains elem, and adds it to the set otherwise.

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


;; takes a sequence as a parameter and returns true if sequence contains some element multiple times.
;; Otherwise it returns false.

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (= (count a-set) (count a-seq)))))


;; takes a book with the previous representation (authors in a vector) and
;; returns the same book in the new representation (authors in a set).

(defn old-book->new-book [book]
  (let [{old-authors :authors} book]
    (assoc book :authors (set old-authors))
    ))


;; returns true if author is in the authors of book and otherwise false.

(defn has-author? [book author]
  (let [{authors :authors} book]
    (contains? authors author)))


;; returns the authors of every book in books as a set.

(defn authors [books]
  (let [author-in-book (fn [book]
                         (:authors book))]
    (apply clojure.set/union (map author-in-book books))))


(defn all-author-names [books]
  (map :name (authors books)))


;;  returns a string representation of author

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author]
    (if (or birth-year death-year)
      (let [str-birth-year (if true? birth-year "")
            str-death-year (if true? death-year "")]
        (str name "(" str-birth-year " - " str-death-year ")"))
      name)
    ))


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
