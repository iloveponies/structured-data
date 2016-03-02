(ns structured-data)

(defn do-a-thing [x]
  (let [doubled-x (+ x x)]
    (Math/pow doubled-x doubled-x)))

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

(defn width [[[bottom-leftX][top-rightX]]]
  (- top-rightX bottom-leftX))

(defn height [[[bottom-leftX bottom-leftY]
               [top-rightX top-rightY]]]
  (- top-rightY bottom-leftY))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[bottom-leftX bottom-leftY]
                        [top-rightX top-rightY]]
                       [pointX pointY]]
  (and (<= bottom-leftX
           pointX
           top-rightX)
       (<= bottom-leftY
           pointY
           top-rightY)))

(defn contains-rectangle? [outer [innerBottomLeft innerTopRight]]
  (and (contains-point? outer innerBottomLeft)
       (contains-point? outer innerTopRight)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [updated-authors (conj (:authors book) new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [element-length (fn [element] (count element))]
    (map element-length collection)))

(defn second-elements [collection]
  (let [second-element (fn [vector] (get vector 1))]
    (map second-element collection)))

(defn titles [books]
  (let [book-title (fn [book] (:title book))]
    (map book-title books)))

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
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [book-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map book-authors books))))

(defn all-author-names [books]
  (let [author-name (fn [author] (:name author))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [author-name (:name author)
        author-years (str (:birth-year author)
                          " - "
                          (:death-year author))]
    (if (:birth-year author)
      (str author-name " (" author-years ")")
      author-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count (count books)
        book-list (apply str (interpose ". " (map book->string books)))]
  (cond
    (== book-count 1) (str book-count " book. " book-list ".")
    (> book-count 1) (str book-count " books. " book-list ".")
    :else "No books.")))

(defn books-by-author [author books]
  (let [book-contains-author?
        (fn [book] (has-author? book author))]
    (filter book-contains-author? books)))

(defn author-by-name [name authors]
  (let [author? (fn [author] (= (:name author) name))
        filtered-authors (filter author? authors)]
    (if (not (empty? filtered-authors))
      (first filtered-authors)
      nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%


