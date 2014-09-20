(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

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
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (if (<= 1 (count books))
    (clojure.set/union (:authors (first books)) (authors (rest books)))
    (:authors books)))

(defn author-names [authorset]
  (if (< 1 (count authorset))
    (clojure.set/union (set [(:name (first authorset))]) (author-names (rest authorset)))
    (set [(:name (first authorset))])))

(defn all-author-names [books]
  (cond
   (< 1 (count books)) (clojure.set/union (author-names (authors [(first books)])) (all-author-names (rest books)))
   (= 1 (count books)) (author-names (authors books))
   :else #{}))

(defn author->string [author]
  (let [name (:name author)
        year (cond
              (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
              (contains? author :birth-year) (str " (" (:birth-year author) " - )")
              :else (str ""))]
    (str name year)
    ))

(defn authors->string [authors]
  (cond
   (< 1 (count authors)) (str (author->string (first authors)) ", " (authors->string (rest authors)))
   (== 1 (count authors)) (author->string (first authors))
   :else (str ""))
  )

(defn book->string [book]
  (if (nil? book) (str "") (str (:title book) ", written by " (authors->string (authors [book])))))

(defn books->string_help [books]
  (cond
   (< 1 (count books)) (str (book->string (first books)) ". " (books->string_help (rest books)))
   (== 1 (count books)) (str (book->string (first books)) ".")
   :else (str "")))

(defn books->string [books]
  (cond
   (< 1 (count books)) (str (count books) " books. " (books->string_help books))
   (== 1 (count books)) (str (count books) " book. " (books->string_help books))
   :else (str "No books.")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [aa (filter (fn [x] (= (:name x) name)) authors)]
      (if (empty? aa) nil (first aa))))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (let [la (living-authors (:authors book))]
    (not(empty? la))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
