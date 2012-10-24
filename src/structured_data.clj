(ns structured-data)

(defn do-a-thing [x] 
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v] 
  (let [a (get v 0)
        b (get v 2)]
         (+ a b)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] 
  (let [[x y z] v]
      (+ x z)))

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
    (if (== (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
    (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (<= x1 x3) (>= x2 x4) (<= y1 y3) (>= y2 y4))))

(defn title-length [book] (count (get book :title)))

(defn author-count [book] (count (get book :authors)))

(defn multiple-authors? [book] (if (> (author-count book) 1) true false))

(defn add-author [book new-author] 
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author] 
  (let [death-year (get author :death-year)]
    (not death-year)))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] 
  (let [take2nd (fn [x] (get x 1))]
    (map take2nd collection)))

(defn titles [books] 
   (map :title books))

(defn monotonic? [a-seq] 
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n] 
  (apply str (repeat n "*")))

(defn toggle [a-set elem] 
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq] 
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book] 
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))))

(defn has-author? [book author] 
  (let [authors (book :authors)]
    (contains? authors author)))

(defn authors [books] 
    (apply clojure.set/union (map :authors books)))

(defn all-author-names [books] 
  (set (map :name (authors books))))

(defn author->string [author] 
  (let [name (author :name) 
        byear (author :birth-year)
        dyear (author :death-year)]
      (if (author :birth-year) (str name " (" byear " - " dyear ")") name)))

(defn authors->string [authors] 
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book] 
  (let [title (book :title)
        auths (book :authors)]
    (str title ", written by " (authors->string auths))))

(defn books->string [books] 
  (let [novels (apply str (interpose ". " (map book->string books)))
        number (count books)]
    (cond
     (== number 0) "No books."
     (== number 1) (str "1 book. " novels ".")
     :else (str number " books. " novels "."))))

(defn books-by-author [author books] 
  (let [author? (fn [book] (has-author? book author))]
  (filter author? books)))

(defn author-by-name [name authors] 
  (let [is-here? (fn [x] (= name (:name x)))]
    (first (filter is-here? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [live-ones (living-authors (book :authors))]
    (if (empty? live-ones) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))