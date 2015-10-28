(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [v1 (get v 0)
        v2 (get v 2)]
    (+ v1
       (if v2
         v2 0))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v1 vp v2] v]
    (+ v1
       (if v2
         v2 0))))

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

(defn area [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn square? [rectangle]
  (= (height rectangle)
     (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[zx zy] point
        [[x1 y1] [x2 y2]] rectangle]
    (and
      (<= x1 zx x2)
      (<= y1 zy y2))))

(defn contains-rectangle? [outer inner]
   (let [[p1 p2] inner]
     (and 
       (contains-point? outer p1)
       (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        size (count authors)]
    (assoc book :authors
      (assoc authors size new-author))))
  
(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get2 (fn [x] (get x 1))]
    (map get2 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq))
        (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [year1 (get author :birth-year)
        year2 (get author :death-year)
        name  (get author :name)]
    (if year2
      (str name " (" year1 " - " year2 ")")
      (if year1 
        (str name " (" year1 " - )")
        (str name)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
        authors (authors->string (get book :authors))]
    (str title ", written by " authors)))

(defn books->string [books]
  (cond
   (not books) "No books."   
   (empty? books) "No books."
   :else (let [s   
               (apply str (interpose ", " (map book->string books)))
               size (count books)]
           (if (> size 1)
             (str size " books. " s ".")
             (str "1 book. " s ".")))))
             
(defn books-by-author [author books]
  (filter 
    (fn [book] 
      (contains? (get book :authors) author))
    books))

(defn author-by-name [name authors]
  (let [r
        (filter (fn [author] 
                  (= (get author :name) name))
                authors)]
    (first r)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
