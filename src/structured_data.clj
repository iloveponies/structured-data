(ns structured-data)

(defn do-a-thing [x]
  (let [doubledX (+ x x)]
    (Math/pow doubledX doubledX)))

(defn spiff [v]
  (if (>= (count v) 3)
    (+ (get v 0) (get v 2))
    nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (>= (count v) 3)
    (let [[x y z] v]
      (+ x z))
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
  (if (== (- top-rightX bottom-leftX) (- top-rightY bottom-leftY))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[bottom-leftX bottom-leftY][top-rightX top-rightY]] [pointX pointY]]
  (if (and (<= bottom-leftX pointX top-rightX) (<= bottom-leftY pointY top-rightY))
    true
    false))

(defn contains-rectangle? [outer [innerBottomLeft innerTopRight]]
  (if (and (contains-point? outer innerBottomLeft) (contains-point? outer innerTopRight))
    true
    false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [updatedAuthors (conj (:authors book) new-author)]
    (assoc book :authors updatedAuthors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn getLength [element]
  (count element))

(defn element-lengths [collection]
  (map getLength collection))

(defn second-elements [collection]
  (let [getSecond (fn [vector] (get vector 1))]
    (map getSecond collection)))

(defn titles [books]
  (let [getBookTitle (fn [book] (:title book))]
    (map getBookTitle books)))

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

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


