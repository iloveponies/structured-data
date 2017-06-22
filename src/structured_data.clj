(ns structured-data)
 
(defn do-a-thing [x]
  (let[xx (+ x x)]
    (Math/pow xx xx)))
 
(defn spiff [v]
  (+(get v 0)(get v 2)))
 
 
 
(defn cutify [v]
  (conj v "<3"))
 
 
 
(defn spiff-destructuring [v]
  (let[[x y z] v]
    (+ x z)))
 
(defn point [x y]
  [x y])
 
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])
 
(defn width [rectangle]
  (let[[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))
 
(defn height [rectangle]
  (let[[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))
 
(defn square? [rectangle]
  (== (width rectangle)(height rectangle)))
 
 
(defn area [rectangle]
  (* (width rectangle)(height rectangle)))
 
(defn contains-point? [rectangle point]
  (let[[[x1 y1][x2 y2]] rectangle [x y] point]
    (and(<= x1 x x2)(<= y1 y y2))))
 
(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and(contains-point? outer point1)(contains-point? outer point2))))
 
(defn title-length [book]
  (count (book :title)))
 
(defn author-count [book]
  (count (book :authors)))
 
(defn multiple-authors? [book]
  (> (author-count book) 1))
 
(defn add-author [book new-author]
  (assoc book :authors(conj (:authors book) new-author)))
 
(defn alive? [author]
  (not (contains? author :death-year)))
 
(defn element-lengths [collection]
  (map count collection))
 
(element-lengths [[1 2 3] [2 2 ] "asd"])
 
(defn second-elements [collection]
  (let [helper(fn [x] (get x 1))]
    (map helper collection)))
 
(defn titles [books]
  (map :title books))
 
(defn monotonic? [a-seq]
  (or (apply <= a-seq)(apply >= a-seq)))
 
(defn stars [n]
  (apply str(repeat n "*")))
 
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))
 
(defn contains-duplicates? [a-seq]
  (> (count a-seq)(count(set a-seq))))
 
(defn old-book->new-book [book]
  (assoc book :authors(set (:authors book))))
 
(defn has-author? [book author]
  (contains? (:authors book) author))
 
(defn authors [books]
  (apply clojure.set/union (map :authors books)))
 
(defn all-author-names [books]
  (set (map :name (authors books))))
 
(defn author->string [author]
  (cond
   (contains? author :death-year)(str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
   (contains? author :birth-year)(str (:name author) " (" (:birth-year author) " - )")
   :else (str (:name author))))
 
(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))
 
(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string(:authors book))))
 
(defn books->string [books]
  (cond
   (== (count books) 1)(str (count books) " book. " (apply str(interpose ", " (map book->string books)))".")
   (> (count books) 1)(str (count books) " books. " (apply str(interpose ", " (map book->string books)))".")
   :else (str "No books.")))
 
(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))
 
(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))
 
(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))
 
(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))
 
(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
