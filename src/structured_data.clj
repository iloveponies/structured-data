(ns structured-data)

(defn
  do-a-thing [tale]
  "Take a number, multiply it by 2, and then raise an obtained value
    into a power of obtained value."
  (let
    [fairy (+ tale tale)]
    (Math/pow fairy fairy)))


(defn
  spiff [vector]
  "A really naive spiff."
  (+
    (get vector 0)
    (get vector 2)))

(defn
  cutify [vector]
  "Make a given vector a little more cute."
  (conj vector "<3"))

(defn
  spiff-destructuring [vector]
  "Exactly the same spiff, by desturcturing input vector."
  (let [[ first! second! third!] vector] (+ first! third!)))

(defn point [x y]
  "This defines a vector!"
  [x y])

(defn rectangle
  "This defines a rectangle!"
  [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  "Compute width of given rectangle."
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1) ))

(defn height [rectangle]
  "Compute height of given rectangle."
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1) ))

(defn square? [rectangle]
  "Detect whether this rectangle square, or return false otherwise."
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  "Compute area of this rectangle."
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  "Detect whether this rectangle contains given point."
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point] (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  "Detect whether outer rectangle contains inner (?) rectangle."
  (let
    [[point1 point2] inner](and (contains-point? outer point1)
                                (contains-point? outer point2))))

(defn title-length [book]
  "Deduce number of letters in a title of this book."
  (count (get book :title)))

(defn author-count [book]
  "Get number of authors this book has."
  (count (get book :authors)))

(defn multiple-authors? [book]
  "Detect, whether this book has multiple authors, or not."
  (< 1 (author-count book)))

(defn add-author [book new-author]
  "Add new author to given book."
  (let
    [original-authors (get book :authors)]
    (assoc book :authors (conj original-authors new-author))))

(defn alive? [author]
  "Detect whether this author had kick a bucket already."
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  "Returns a collection, which contains lengths of elements in a given collection."
  (map count collection))

(defn second-elements [collection]
  "Return a collection, which contains lengths of elements in a given collection."
  (let
    [get-second-element (fn [collection-element] (get collection-element 1))]
    (map get-second-element collection)))

(defn titles [books]
  "Returns titles of books."
  (map :title books))

(defn monotonic? [a-seq]
  "Returns true if a-seq is monotonic, false otherwise."
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [in-finite]
  "Returns in-finite stars!"
  (apply str (repeat in-finite "*")))

(defn toggle [a-set elem]
  "Remove element from a-set, if it is contained in a-set, and add it otherwise."
  (apply (if (contains? a-set elem) disj conj) [a-set elem]))

(defn contains-duplicates? [a-seq]
  "Detect whether a-seq contains duplicates."
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  "An old books contain authors in sequences,
  new age (O_o) requires them to be in sets."
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  "Detect, whether this book carries given author among credited."
  (contains? (get (old-book->new-book book) :authors) author))

(defn authors [books]
  "Get all authors of given books collection as a set"
  (set (concat
         (apply clojure.set/union (map :authors books)))) )

(defn all-author-names [books]
  "Get all author names for this books collection."
  (set (map :name (authors books))))

(defn author->string [author]
  "Write down author name and lifespan period; if author has no known lifespan, omit that info."
  (let
    [name (get author :name)
     years (if
             (contains? author :birth-year)
             (str " (" (get author :birth-year) " - " (get author :death-year) ")")
             "")
     ]
    (str name years)))

(defn authors->string [authors]
  "Write down author strings, comma separated."
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  "Write down book string representation, suitable for marketing it (o)n line!"
  (str (get book :title) ", written by " (authors->string (get book :authors)))
  )

(defn books->string [books]
  "Write down a very special string representation for given books set, preceded by count of those."
  (
    let [
         number-books (count books)
         no-books (== number-books 0)
         count-suffix (cond
                       no-books "No books."
                       (== number-books 1) (str number-books " book. ")
                       :else (str number-books " books. "))
         postfix (if no-books "" ".")
         ]
    (apply str (concat [count-suffix] (interpose ". " (map book->string books)) [postfix]))
    )
  )

(defn books-by-author [author books]
  "Find books, written down by this author."
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  "Get author by name."
  (first (filter (fn [author] (= (get author :name) name)) authors))
  )

(defn living-authors [authors]
  "Get list of authors, who were brave enough to survive till the marvellous present time."
  (filter alive? authors))

(defn has-a-living-author? [book]
  "Detect whether this book have really brave author among those who write it."
  (not (empty? (living-authors (authors [book]))))
  )

(defn books-by-living-authors [books]
  "Get books, which are written by brave authors!"
  (filter has-a-living-author? books))

; ^________^
