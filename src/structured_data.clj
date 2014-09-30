(ns structured-data)

(defn do-a-thing [x]
  (let [tw (+ x x)]
    (Math/pow tw tw)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bl-x bl-y] [tr-x tr-y]]]
  (- tr-x bl-x))

(defn height [[[bl-x bl-y] [tr-x tr-y]]]
  (- tr-y bl-y))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[bl-x bl-y] [tr-x tr-y]] [x y]]
  (and (<= bl-x x tr-x) (<= bl-y y tr-y)))

(defn contains-rectangle? [outer [bottom-left top-right]]
  (and (contains-point? outer bottom-left)
       (contains-point? outer top-right) ) )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [zweitel (fn [x] (second x))]
     (map zweitel collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set(:authors book)))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map :name (apply concat (map :authors books)))))

(defn author->string [author]
  (let [name (:name author) birth (:birth-year author) death (:death-year author)]
    (if birth
      (str name " (" birth " - " death ")")
      (str name)) ) )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [new-book (old-book->new-book book)
        title (:title new-book)
        authors (:authors new-book)
        authors-str (authors->string authors)]
    (str title ", written by " authors-str)))

(defn books->string [books]
  (let [book-strings (reverse (reverse (map book->string books)))
        number (count book-strings)]
    (cond (= number 0) "No books."
          (= number 1) (str "1 book. " (first book-strings) ".")
          true (str number " books. " (apply str (interpose ". " book-strings)) ".") ) ) )

(defn books-by-author [author books]
  (let [has-this-author (fn [book] (has-author? book author))]
    (filter has-this-author books)))

(defn author-by-name [name authors]
  (first (filter  (fn [author] (= name (:name author)))  authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (authors (list book))))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
