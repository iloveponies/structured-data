(ns structured-data)
(use 'clojure.set)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (= h w)
    )
  )

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* h w)
    )
  )

(defn contains-point? [rectangle point]
  (let [[[botx boty] [topx topy]] rectangle
        [puntx punty] point]
    (and (<= botx puntx topx) (<= boty punty topy)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[blix bliy] [tlix tliy]] inner
        [[blox bloy] [tlox tloy]] outer]
    (and (>= blix blox) (>= bliy bloy) (<= tlix tlox) (<= tliy tloy)))
  )

(defn title-length [book]
  (count (book :title))
  )

(defn author-count [book]
  (count (book :authors))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false)
  )

(defn add-author [book new-author]
  (let [authors-from-book (:authors book)]
    (assoc book :authors (conj authors-from-book new-author)))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [vec] (get vec 1))]
    seq (map get-second collection))
  )

(defn titles [books]
  seq (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (let [authors-from-book (:authors book)]
    (assoc book :authors (set authors-from-book)))
  )

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author))
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if (contains? author :birth-year )
      (str author-name " (" birth-year " - " death-year ")")
      (str author-name)))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [title (:title book)
    authors (:authors book)]
    (str title ", written by "(authors->string authors)))
  )

(defn books->string [books]
  (let [num (count books)]
    (if (> num 0)
      (let [titles (apply str (interpose ". " (map book->string books)))]
        (if (== num 1)
          (str num " book. " titles ".")
          (str num " books, " titles ".")))
      "No books.")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [match (filter (fn [author]
            (= name (:name author))) authors)]
        (first match)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [found (filter (fn [author] (alive? author)) (:authors book))]
    (boolean (> (count found) 0))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
