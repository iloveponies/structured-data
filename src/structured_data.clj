(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  ( + (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    ( + a c )))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    ( - x2 x1 )))

(defn height [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    ( - y2 y1 )))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (
    let [
      [[x1 y1][x2 y2]] rectangle
      [x0 y0] point
    ]
    (and (<= x1 x0 x2) (<= y1 y0 y2))))

(defn contains-rectangle? [outer inner]
  (
    let [
      [[x1 y1][x2 y2]] outer
      [[x3 y3][x4 y4]] inner
    ]
    ( and ( and ( <= x1 x3 x2 ) ( <= y1 y3 y2 ) ) ( and ( <= x1 x4 x2 ) ( <= y1 y4 y2 ) ) ) ) )


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (
    let [plop (:authors book)]
    (assoc book :authors (conj plop new-author))
  ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn plop [x]
  (count x))

(defn element-lengths [collection]
  (map plop collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map (fn [book] (:title book)) books))

(defn monotonic? [a-seq]
  ( or (apply <= a-seq) (apply >= a-seq) ) )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (not (contains? a-set elem))
    (conj a-set elem)
    (disj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [naks (:authors book)]
    (assoc book :authors (set naks))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map (fn [author] (:name author)) (authors books))))

(defn author->string [author]
  (
    let [
      name (:name author)
      bday (:birth-year author)
      dday (:death-year author)
    ]
    (if ( not (contains? author :birth-year))
    ( str name)
    ( apply str ( concat name " (" (str bday) " - " (str dday) ")")))
  )
)

(defn authors->string [authors]
  ( apply str (interpose ", " (map author->string authors))) )

(defn book->string [book]
  (
    let [
      name (:title book)
    ]
    ( apply str ( concat name ", written by " (authors->string (:authors book) ) ) )
  )
)

(defn books->string [books]
  (
    if ( not ( > ( count books ) 0 ) )
      "No books."
      (
        if( == (count books) 1)
        ( apply str ( concat ( str ( count books ) ) " book. " ( apply str ( map book->string books ) ) "." ) )
        ( apply str ( concat ( str ( count books ) ) " books. " ( apply str ( interpose ". " ( map book->string books ) ) ) "." ) )
      )
  )
)

(defn books-by-author [author books]
  (
    filter (fn [book] (has-author? book author)) books
  )
)

(defn author-by-name [name authors]
  (
    first ( filter (fn [author] ( == (compare (:name author) name) 0 ) ) authors )
  )
)

(defn living-authors [authors]
  (
    filter (fn [author] (alive? author) ) authors
  )
)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
