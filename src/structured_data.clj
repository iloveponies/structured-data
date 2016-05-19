(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [f (get v 0)
        t (get v 2)]
    (+ f t)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [ [f _ t] v]
    (+ f t)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle
         [x y] point ]
     (and (<= x1 x x2)
          (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [ [bottom-left top-right] inner ]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (let [title (:title book)]
     (count title)))

(defn author-count [book]
  (let [authors (:authors book)]
     (count authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondary (fn [x] (get x 1))]
    (map secondary collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond 
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else            false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
     (disj a-set elem)
     (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [orig-count (count a-seq)
        uniq-count (count (set a-seq))]
    (if (< uniq-count orig-count) true false)))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        author-set (set authors)]
     (assoc book :authors author-set)))

(defn has-author? [book author]
  (let [book-authors (:authors book)]
    (if (contains? book-authors author) true false)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [ author-name (:name author)
         birth (:birth-year author)
         death (:death-year author)]
    (cond
      (nil? birth) (str author-name)
      :else (str author-name " (" birth " - " death ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [ title (:title book)
         author-string (authors->string (:authors book))]
     (str title ", written by " author-string)))

(defn books->string [books]
  (let [ items (count books)
         book-string (apply str (interpose ". " (map book->string books)))  
         label (if (= 1 items) "book. " "books. ")]
    (if (= 0 items) 
      "No books."
      (str items " " label book-string "."))))

(defn books-by-author [author books]
  (let [author-check-fn (fn [x] (has-author? x author))]
    (filter author-check-fn books)))

(defn author-by-name [name authors]
  (let [match-author (fn [x] (if (= name (:name x)) true false))]
    (first (filter match-author authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        living-authors (filter alive? authors)]
     (not (empty? living-authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
