(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
    (Math/pow x+x x+x)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))


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
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (= (width rectangle) (height rectangle))))

(defn area [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (* (width rectangle) (height rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle]
    (let [[px py] point]
      (and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[ip1 ip2] inner]
    (and (contains-point? outer ip1) (contains-point? outer ip2))))

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if(contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn mungefy [a-seq]
  (let [munge (fn [x] (+ x 42))]
    (map munge a-seq)))

(defn second-elements [collection]
  (let [getSecond (fn [x] (get x 1))]
    (map getSecond collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (=(count a-set)(count a-seq)))))

(defn old-book->new-book [book]
  (let [author-names (:authors book)]
    (assoc book :authors (set author-names))))

(defn has-author? [book author]
  (let [author-names (:authors (old-book->new-book book))]
    (contains? author-names author)))

(defn authors [books]
  (let [authors (fn[x] (:authors x))]
    (set (apply concat (map authors books)))))

(defn all-author-names [books]
  (let [author-names (fn[x] (:name x))]
    (set (map author-names (authors books)))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [by (:birth-year author)]
      (let [years (if(not (nil? by))
                    (str " (" by " - " (:death-year author) ")")
                    (str ""))]
      (str author-name years)))))

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))


(defn book->string [book]
  (let [title (:title book)]
    (let [authors (authors->string (:authors book))]
      (str title ", written by " authors))))

(defn books->string [books]
  (let [books-string (apply str (interpose ". " (map book->string books)))]
    (cond
      (empty? books) "No books."
      (= (count books) 1) (str "1 book. " books-string ".")
    :else (str (count books) " books. " books-string "."))))

(defn books-by-author [author books]
  (filter (fn[x](has-author? x author)) books))

(defn author-by-name [name authors]
  (first(filter (fn[x](= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn[x](alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors(:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
