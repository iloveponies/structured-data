(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (let [a (width rectangle)
        b (height rectangle)]
    (= a b)))

(defn area [rectangle]
    (let [a (width rectangle)
        b (height rectangle)]
    (* a b)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x3 y3] point]
    (let [a (<= x1 x3 x2)
          b (<= y1 y3 y2)]
      (and a b))))

(defn contains-rectangle? [outer inner]
  (let [[first second] inner]
    (let [a (contains-point? outer first)
          b (contains-point? outer second)]
      (and a b))))

(defn title-length [book]
  (let [a (:title book)]
    (count a)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

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
  (let [a-set (set a-seq)
        diff (- (count a-seq) (count a-set))]
    (if (< 0 diff)
      true
      false)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [a (map :authors books)]
    (apply clojure.set/union a)))

(defn all-author-names [books]
  (let [a (authors books)]
    (let [b (map :name a)]
      (set b))))

(defn author->string [author]
  (let [a (:name author)
        b (:birth-year author)
        c (:death-year author)]
    (if c
      (str a " (" b " - " c ")")
      (if b
        (str a " (" b " - )")
        (str a)))))

(defn authors->string [authors]
  (let [a (map author->string authors)]
    (apply str (interpose ", " a))))

(defn book->string [book]
  (let [a (:title book)
        b (:authors book)]
    (let [c (authors->string b)]
      (str a ", written by " c))))

(defn books->string [books]
  (let [a (map book->string books)]
    (let [b (interpose ", " a)]
      (let [c (apply str b)]

      (if (== 0 (count a))
        (str "No books.")
        (if (>= 1 (count a))
            (str (count a) " book. " c ".")
            (str (count a) " books. " c ".")))))))

(defn books-by-author [author books]
  (let [a (map (has-author? books author) books)]
    (filter a books)))

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (not (contains? x :death-year))) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
