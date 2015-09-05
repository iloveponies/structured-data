(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff[v] 
  (+ (get v 0) (get v 2)))

(defn cutify[v] 
  (conj v "<3"))


(defn spiff-destructuring [v] 
  (let [[a b c] v] (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width[rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] 
    (- x2 x1)))

(defn height[rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] 
    (- y2 y1)))
 
(defn square?[rectangle]
  (= (height rectangle) (width rectangle)))
 
(defn area[rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] 
    (* (- x2  x1)(- y2 y1))))
 
(defn contains-point?[rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (and (<= x1 x x2)(<= y1 y y2))))
 
(defn contains-rectangle?[outer inner]
  (let [[[x1 y1] [x2 y2]] outer [p1 p2] inner]
    (and (contains-point? outer p1)(contains-point? outer p2))))

(defn title-length [book]
   (count (get book :title)))
 
(defn author-count [book]
  (count (get book :authors)))
 
(defn multiple-authors?[book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        add-new-author (conj authors new-author)]
    (assoc book :authors add-new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [fun (fn [v] (get v 1))]
    (map fun collection)))

(defn titles [books]
(let [get-title (fn [book] (:title book))]
    (map get-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [author-names 
        (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [
        name  (:name author)
        birth (:birth-year author)
        death (:death-year author)
        ]
    (if (nil? birth)
      name
      (str name \space \( birth \space \- \space death \)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [
        title (:title book)
        authors (:authors book)
        ]
    (if (nil? authors)
      title
      (str title \, \space "written by" \space (authors->string authors))
      )))

(defn books->string [books]
  (let [counter (fn [books] 
                  (cond 
                   (== 1 (count books)) (str "1 book.")
                   :else (str (count books) " books.")
                   ))]
    
    (if (== 0 (count books))
      "No books."
      (str (counter books) \space 
           (apply str (interpose ". " (map book->string books))) \.)
      )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (set 
          (filter 
           (fn [author] (if (= name (:name author)) true false )) 
           authors))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn[book] (has-a-living-author? book)) books)
  )

; %________%
