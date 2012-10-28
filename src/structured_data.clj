(ns structured-data)

(defn do-a-thing [x]
  (let 
    [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2) ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z)))

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
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] inner]
    (if (and (contains-point? outer [x3 y3])
             (contains-point? outer[x4 y4]))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (conj (:author book) new-author))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [tokarvo (fn [x] (get x 1))]
    (map tokarvo collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (if (apply <= a-seq)
    true
    false)
      (if (apply >= a-seq)
        true
        false)))

(defn stars [n]
  (let [ tahdet (repeat n '*)]
  (apply str tahdet )))

(defn toggle [a-set elem]
   (if (contains? a-set elem)
   	(disj a-set elem)
     (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
   (set book))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
  true
  false))

(defn authors [books]
  (set (map :name (:authors books))))

(defn all-author-names [books]
  :-)

(defn author->string [author]
  (let [syntyma (:birth-year author)
        kuollut (:death-year author)
        nimi (:name author)]
    (if (contains? author :birth-year) 
      (str nimi " " "("syntyma " - " kuollut")")
      (str nimi)
      )))

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