(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring 
  [[a _ c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle 
  [bottom-left top-right]
  [bottom-left top-right])

(defn width 
  [[[x1 y1] [x2 y2]]]
  (Math/abs (- x2 x1)))

(defn height
  [[[x1 y1] [x2 y2]]]
  (Math/abs (- y2 y1)))


(defn square? [rectangle]
  (= (height rectangle)
     (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) 
     (height rectangle)))

(defn contains-point? 
  [[[x1 y1][x2 y2]] [px py]]
  (or (and (>= x1 px x2) (>= y1 py y2))
      (and (>= x2 px x1) (>= y2 py y1))
))

(defn contains-rectangle? 
  [outer [lower-left top-right]]
  (and (contains-point? outer lower-left)
       (contains-point? outer top-right)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [v-authors (:authors book)]
    (assoc book :authors (conj v-authors new-author))))

(defn alive? [author]
  (not  (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [extract (fn [v] (get v 1))]
    (map extract collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or  (apply <= a-seq)
       (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if  (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (apply conj #{} (:authors book))))


(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors (clojure.set/union books)))))

(defn all-author-names [books]
  (set  (map :name (authors books))))

(defn author->string [author]
  (let [death-year (:death-year author)
        birth-year (:birth-year author)
        author-name (:name author)]
    (cond 
      death-year (str author-name " (" birth-year " - " death-year ")")
      birth-year (str author-name " (" birth-year " - )" )
      :default author-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [number (count books)
        no-str (condp = number 0 "No books." 1 "1 book. " (str number " books. "))
        ->string (apply str (interpose ". "  (map book->string books)))]
    (if (= number 0)
       no-str
       (str no-str ->string "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
