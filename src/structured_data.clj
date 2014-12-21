(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)
  ))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]              ;Is there any way to skip the y here?
  (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1] [x2] ] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]]  rectangle]
  (- y2 y1))
  )

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (== h w))
  )

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* h w))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl) (contains-point? outer tr)))
  )

(defn title-length [book]
  ( count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
  (let [authors (:authors book)
        newA (conj authors new-author)]
    (assoc book :authors newA))
  )


(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [take-two (fn [v] (second v))]
    (map take-two collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (let [v (repeat n "*")]
    (apply str v))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (let [newA (set (:authors book))]
    (assoc book :authors newA))
  )

(defn has-author? [book author]
  (let [as (:authors book)]
    (contains? as author))
  )

(defn authors [books]
  (let [autSet (map :authors books)]
    (apply clojure.set/union autSet))
  )

(defn all-author-names [books]
  (let [aus (authors books)]
    (set (map :name aus)))
  )

(defn author->string [author]
  (let [name (:name author)
        by (:birth-year author)
        dy (:death-year author)]
    (if (not by)
    name
    (str name " (" by " - " dy ")")))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )


(defn book->string [book]
    (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  ( cond
    (not books) "No books."
    (== 0 (count books)) "No books."
    (== 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else (str (count books) " books. " (apply str (interpose ". "(map book->string books))) ".")
  )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [auth] (= name (:name auth))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
    (not (empty? (filter alive? (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
