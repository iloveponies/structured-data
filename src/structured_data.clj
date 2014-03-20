(ns structured-data)

(defn do-a-thing [x]
  (let [doubl (+ x x)]
    (Math/pow doubl doubl)))

(defn spiff [v]
  (+
    (get v 0)
    (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x, _, z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x _] [a _]]]
  (- a x))

(defn height [[[_ y] [_ b]]]  
  (- b y))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x y] [a b]] [i j]]
  (and (<= x i a)
       (<= y j b)))

(defn contains-rectangle? [rectangle [bottom-left top-right]]
  (and (contains-point? rectangle bottom-left)
       (contains-point? rectangle top-right)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        updated-authors (conj authors new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second-elem (fn [[_, y]] y)]
    (map get-second-elem collection)))

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
  (let [a-set (set a-seq)]
    (not (== (count a-seq) (count a-set)))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)
        new-authors (set authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)))

(defn authors [books]
  (let [get-authors (fn [book] (get book :authors))]
    (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (let [authori (authors books)
        get-author-name (fn [author] (get author :name))]
    (set (map get-author-name authori))))

(defn author->string [author]
  (let [name (get author :name)
        birth-year (get author :birth-year)
        death-year (get author :death-year)
        years (str " (" birth-year " - " death-year ")")]
    (if (contains? author :birth-year)      
      (str name years)
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
        authors (get book :authors)
        author-names (authors->string authors)]
    (str title ", written by " author-names)))

(defn books->string [books]
  (let [book-count (count books)
        book-string (if (> book-count 1) "books" "book")
        book-strings (map book->string books)
        books-string (apply str (interpose ". " book-strings))]
    (if (> book-count 0)
      (str book-count " " book-string ". " books-string ".")
      (str "No books."))))

(defn books-by-author [author books]
  (let [func (fn [book] (has-author? book author))]
    (filter func books)))

(defn author-by-name [name authors]
  (let [func (fn [author] (= name (get author :name)))]
    (first (filter func authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (get book :authors)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
