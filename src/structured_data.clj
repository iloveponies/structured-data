(ns structured-data)

(defn do-a-thing [x]
  (let [doubledX (+ x x)]
    (Math/pow doubledX doubledX)))

(defn spiff [v]
  (if (>= (count v) 3)
    (+ (v 0) (v 2))
    nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (>= (count v) 3)
    (let [[first second third] v]
      (+ first third))
    nil))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bottom-leftX bottom-leftY][top-rightX top-rightY]]]
  (- top-rightX bottom-leftX))

(defn height [[[bottom-leftX bottom-leftY][top-rightX top-rightY]]]
  (- top-rightY bottom-leftY))

(defn square? [[[bottom-leftX bottom-leftY][top-rightX top-rightY]]]
  (== (- top-rightX bottom-leftX) (- top-rightY bottom-leftY)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[bottom-leftX bottom-leftY][top-rightX top-rightY]] [pointX pointY]]
  (and (<= bottom-leftX pointX top-rightX) (<= bottom-leftY pointY top-rightY)))

(defn contains-rectangle? [outer [innerBottomLeft innerTopRight]]
  (and (contains-point? outer innerBottomLeft) (contains-point? outer innerTopRight)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [updatedAuthors (conj (:authors book) new-author)]
    (assoc book :authors updatedAuthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [getLength (fn [element] (count element))]
    (map getLength collection)))

(defn second-elements [collection]
  (let [getSecond (fn [vector] (get vector 1))]
    (map getSecond collection)))

(defn titles [books]
  (let [getBookTitle (fn [book] (:title book))]
    (map getBookTitle books)))

(defn monotonic? [a-seq]
  (let [increasing?
        (fn [seq]
          (<= (seq 0) (seq 1)))]
    (if (increasing? a-seq)
      (apply <= a-seq)
      (apply >= a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authorsSet (set (:authors book))]
    (assoc book :authors authorsSet)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [getAuthors (fn [book] (:authors book))]
    (apply clojure.set/union (map getAuthors books))))

(defn all-author-names [books]
  (let [getNames (fn [author] (:name author))]
    (set (map getNames (authors books)))))

(defn author->string [author]
  (let [authorName (:name author)
        authorYears (str (:birth-year author) " - " (:death-year author))]
    (if (:birth-year author)
      (str authorName " (" authorYears ")")
      authorName)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookCount (count books)
        bookList (apply str (interpose ". " (map book->string books)))]
  (cond
    (== bookCount 1) (str bookCount " book. " bookList ".")
    (> bookCount 1) (str bookCount " books. " bookList ".")
    :else "No books.")))

(defn books-by-author [author books]
  (let [booksByAuthor
        (fn [book] (has-author? book author))]
    (filter booksByAuthor books)))

(defn author-by-name [name authors]
  (let [isAuthor (fn [author] (= (:name author) name))
        filteredAuthors (filter isAuthor authors)]
    (if (not (empty? filteredAuthors))
      (first filteredAuthors)
      nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (let [hasLivingAuthor (fn [book] (has-a-living-author? book))]
    (filter hasLivingAuthor books)))

; %________%


