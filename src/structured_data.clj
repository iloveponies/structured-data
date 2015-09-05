(ns structured-data)

(defn do-a-thing [x]
  (let [ xx (+ x x) ]
    (Math/pow xx xx)))

(defn spiff [v]
  (+
   (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [ [x - y] v ]
    (+ (v 0) (v 2))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))) 

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [ [bl tr] inner ]
    (and
      (contains-point? outer bl)
      (contains-point? outer tr))))

(defn title-length [book]
  (count (book :title)))

(defn author-count [book]
  (count (book :authors)))

(defn multiple-authors? [book]
  (not= (author-count book) 1))

(defn add-author [book new-author]
  (let [ authors (book :authors) ]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*"))) 

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [t (set a-seq)]
    (not= (count a-seq) (count t))))

(defn old-book->new-book [book]
  (let [ authors (set (book :authors)) ]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union 
         (map :authors books)))

(defn all-author-names [books]
        (set (map :name (authors books))))

(defn author->string [author]
   (let [end (if (author :birth-year)
             (str " ("  (author :birth-year) 
                  " - " (author :death-year)
                  ")")
              "")]
     (str (author :name) end)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (cond
    (= (count books) 0) "No books."
    (= (count books) 1) (str "1 book. " (book->string (books 0)) ".")
    :else
      (str (count books) " books. " 
           (apply str (interpose ". " (map book->string books)))
           ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (author :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
