(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[_1st _2nd _3rd] v]
    (+ _1st _3rd)))

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
  (let [w (width rectangle)
        h (height rectangle)]
    (== h w)))


(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* h w)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner
        tl [x1 y1]
        br [x2 y2]]
    (and (contains-point? outer tl) (contains-point? outer br))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [xs]
  (map count xs)
  )

(defn second-elements [collection]
  (let [extract (fn [xs] (get xs 1))]
    (map extract collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [xs]
  (or (apply <= xs) (apply >= xs)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str (:birth-year author) " - " (:death-year author))]
    (if (:birth-year author)
      (str name " (" years ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [nb (count books)]
    (cond
     (== 0 nb) "No books."
     (== 1 nb) (str "1 book. " (book->string (get books 0)) ".")
     :else (str nb " books. " (apply str (interpose ". " (map book->string books)))))
    ))

(defn books-by-author [author books]
  (let [author? (fn [book] (< 0 (count (filter (fn [x] (= author x)) (:authors book)))))]
    (filter author? books)
    ))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name) ) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
