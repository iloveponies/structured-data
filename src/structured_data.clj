(ns structured-data)

(defn do-a-thing [x]
  (let [x-plus-x (+ x x)]
    (Math/pow x-plus-x x-plus-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1][rx2 ry2]] rectangle
        [x y] point]
    (and
     (<= rx1 x rx2)
     (<= ry1 y ry2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left-inner top-right-inner] inner]
    (and
     (contains-point? outer bottom-left-inner)
     (contains-point? outer top-right-inner))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second-element (fn [v] (get v 1))]
    (map get-second-element collection)))

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
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [author-vector (:authors book)
        author-set (set author-vector)]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))))

(defn author->string [author]
  (let [name (:name author)
        years (if (:birth-year author)
                (str " (" (:birth-year author) " - " (:death-year author) ")")
                "")]
      (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [number-of-books (count books)
        pluralize-number-of-books (cond
                                   (== 0 number-of-books) "No books."
                                   (== 1 number-of-books) "1 book."
                                   :else (str number-of-books " books."))
        list-of-books (str " " (apply str (interpose ". " (map book->string books))) ".")]
    (str pluralize-number-of-books
         (if (> number-of-books 0)
           list-of-books
           ""))))

(defn books-by-author [author books]
  (let [book-has-given-author? (fn [book] (has-author? book author))]
  (filter book-has-given-author? books)))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%

