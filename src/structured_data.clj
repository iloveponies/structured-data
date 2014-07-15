(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff [v]
  (let [first-item (get v 0)
        third-item (get v 2)]
    (+ first-item third-item)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first-item _ third-item] v]
    (+ first-item third-item)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and
         (<= x1 xp x2)
         (<= y1 yp y2))
      true
      false
      )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (if (and
         (contains-point? outer inner-bottom-left)
         (contains-point? outer inner-top-right))
      true
      false)
    ))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let
    [mod-authors (conj (:authors book) new-author)]
    (assoc book :authors mod-authors))
  )

(defn alive? [author]
  ;author is a map with :name, :birth-year, and :death-year keys
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  ; Takes a vector of vectors and returns a sequence of the second element
  ; of each of these vectors.
  (let [sec (fn [vec] (get vec 1))]
    (map sec collection)
    ))

(defn titles [books]
  "Takes a collection of books and returns all authors contained in the collection."
  (map :title books))

(defn monotonic? [a-seq]
  "Determines if a sequence of numbers is monotonic, increasing or decreasing."
  (if (or (apply <= a-seq)
          (apply >= a-seq))
    true
    false)
  )

(defn stars [n]
  "Returns a string with n * symbols."
    (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  "Determines if a sequence has duplicate entries."
  (> (count a-seq) (count (set a-seq)))
    )

(defn old-book->new-book [book]
  "Turns data in the old book format into one that is a set."
  (let [curr-authors (:authors book)]
    (assoc book :authors (set curr-authors))
    )
  )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  "Creates a set of maps that each represent an author."
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  "Return all author names in a set of strings."
  (let [curr-authors (authors books)]
    (set (map :name curr-authors))))

(defn author->string [author]
  (let [author-str (str (:name author))
        birth-year (str (:birth-year author))
        death-year (str (:death-year author))]
    (if (contains? author :birth-year)
      (str author-str " (" birth-year " - " death-year ")")
      author-str)
    ))

(defn authors->string [authors]
  "Generates a single string of authors based on a collection of author map entries."
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title-str (str (:title book))
        authors-str (authors->string (:authors book))]
    (str title-str ", written by " authors-str)
    ))

(defn books->string [books]
  "Turns a collection of books into a readable string."
  (if (empty? books)
    (str "No books.")
    (let [book-count (count books)
          book-str-header (if (> book-count 1)
                            (str book-count " books.")
                            (str book-count " book."))]
      (str book-str-header " " (apply str (interpose ". " (map book->string books))) ".")
      )))

(defn books-by-author [author books]
  "Returns a sequence of books by the provided author."
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [author-names (map :name authors)]
    (first (filter (fn [auth] (= (:name auth) name)) authors))))


(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
