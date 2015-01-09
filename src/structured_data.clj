(ns structured-data)

(defn do-a-thing [x]
  (let [addx (+ x x)]
    (Math/pow addx addx)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  :-)

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[leftmost-x bottom-y] [rightmost-x top-y]] rectangle]
    (- rightmost-x leftmost-x))) 

(defn height [rectangle]
  (let [[[leftmost-x bottom-y] [rightmost-x top-y]] rectangle]
    (- top-y bottom-y)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[left-x bottom-y] [right-x top-y]] rectangle
        [x y] point]
    (and (<= left-x x right-x) (<= bottom-y y top-y))))

(defn contains-rectangle? [outer inner]
  (let [[[outer-left-x outer-bottom-y] [outer-right-x outer-top-y]] outer
        [[inner-left-x inner-bottom-y] [inner-right-x inner-top-y]] inner]
    (and (contains-point? outer [inner-left-x inner-bottom-y])
         (contains-point? outer [inner-right-x inner-top-y]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [orig-authors (:authors book)
        new-authors (conj orig-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsecond (fn [x] (get x 1))]
    (map getsecond collection)))

(defn titles [books]
  (map :title books))
  

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set (concat old-authors)))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-list (authors books)]
    (set (map :name author-list))))
  
(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
      death-year (str author-name " (" birth-year " - " death-year ")")
      birth-year (str author-name " (" birth-year " - )")
      :else author-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)
        authorstring (authors->string authors)]
    (apply str (interpose ", written by " [title authorstring]))))

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
