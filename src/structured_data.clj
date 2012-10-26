(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and
     (<= x1 p1 x2)
     (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and
     (contains-point? outer point1)
     (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new-author-list (conj (:authors book) new-author)]
    (assoc book :authors new-author-list)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [take-second (fn [vector] (get vector 1))]
    (map take-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)
      (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [vuosi (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")")
                nil)]
    (str (:name author) vuosi)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [writer (if (contains? book :authors)
                      (str ", written by " (authors->string (:authors book)))
                      nil)]
    (str (:title book) writer)))

(defn books->string [books]
  (let [number (count books)
        how-many (cond
                  (== number 0) "No books."
                  (== number 1) "1 book."
                  :else (str number " books."))
        book-descriptions-coll (map book->string books)
        coll-with-points (interpose ". " book-descriptions-coll)
        stringed-coll (apply str coll-with-points)
        stringed-coll-added (if (> (count books) 0) (str " " stringed-coll ".") (stringed-coll))]
    (str how-many stringed-coll-added)))

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
