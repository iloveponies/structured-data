(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (* 2 x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (v 0) (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v1 _ v2]]
  (+ v1 v2))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [n] 
  (if (< n 0) (- n) n))

(defn width [[[x1 _] [x2 _ ]]]
  (abs (- x1 x2)))

(defn height [[[_ y1] [_ y2]]]
  (abs (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x? y?]]
  (and (<= x1 x? x2) (<= y1 y? y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1) (contains-point? outer p2)))

(defn title-length [{title :title}]
  (count title))

(defn author-count [{authors :authors}]
  (count authors))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [{authors :authors :as book} new-author]
  (let [authors (conj authors new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [{authors :authors :as book}]
  (assoc book :authors (set authors)))

(defn has-author? [{authors :authors} author]
  (contains? authors author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [{name :name birth-year :birth-year death-year :death-year}]
  (str name 
       (when birth-year (str " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [{title :title authors :authors}]
  (str title ", written by " (authors->string authors)))

(defn books->string [books]
  (let [book-count (count books)]
    (if (zero? book-count) 
      "No books."
      (str book-count " book" (when (> book-count 1) \s) "."
           " " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [{authors :authors}] (contains? authors author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [{n :name}] (= n name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [{authors :authors}]
  (boolean (seq (living-authors authors))))
  

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
