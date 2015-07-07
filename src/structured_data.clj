(ns structured-data)

(defn do-a-thing [x]
  (let [z (+ x x)]
    (Math/pow z z)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    str (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle
  [bottom-left top-right] [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
        (let [[[x1 y1] [x2 y2]] rectangle]
          (= (- y2 y1) (- x2 x1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (* (- y2 y1) (- x2 x1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point] (and (>= x2 x x1) (>= y2 y y1))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [original (:authors book)
        new (conj original new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [length (fn [x] (count x))]
        (map length collection)))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (let [tahdet (repeat n "*")]
    (apply str tahdet)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [setti (set a-seq)]
    (< (count setti) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [kirjailijat (fn [book] (:authors book))]
    (apply clojure.set/union (map kirjailijat books))))

(defn all-author-names [books]
  (let [kirjailijat (authors books)]
    (set (map :name kirjailijat))))

(defn author->string [author]
  (let [name (:name author)
        syntyma (:birth-year author)
        kuolema (:death-year author)]
    (if (:birth-year author) (str name " (" syntyma  " - " kuolema ")") (str name))))

(defn authors->string [authors]
  (let [kirjailijat (map author->string authors)]
    (apply str (interpose ", " kirjailijat))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [number-of-books (count books)
        books-as-string (apply str (interpose ". " (map book->string books)))]
    (if (= number-of-books 0) (str "No books.") (if (= number-of-books 1) (str number-of-books " book. " books-as-string ".") (str number-of-books " books. " books-as-string ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (< 0 (count (filter (fn [author] (alive? author)) (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
