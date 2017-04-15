(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
     (Math/pow x+x x+x)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))

(spiff [1 2])         ;=> nullpointer exception
(spiff [])            ;=> nullpointer exception

(defn cutify [v]
  (conj v "<3"))


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
   (= (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle]
      (let [[px py] point]
        (and (<= x1 px x2) (<= y1 py y2)))))


(defn contains-rectangle? [outer inner]
  (let [[[ix1 iy1] [ix2 iy2]] inner]
        (and (contains-point? outer (point ix1 iy1))
             (contains-point? outer (point ix2, iy2)))))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (< 1 (author-count book)))


(defn add-author [book new-author]
  (let [original (:authors book)]
    (assoc book :authors (conj original new-author))))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
 (map count collection))


(defn second-elements [collection]
  (let [secs (fn [x] (get x 1))]
    (map secs collection)))


(defn titles [books]
  (let [titl (fn [book] (:title book))]
    (map titl books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
   (not (= (count a-seq) (count(set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [auths
         (fn [book] map (:authors book))]
	(set (apply clojure.set/union (map auths books)))))
;HOW?? :D

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (author :name)
       synt (author :birth-year)
        kuol (author :death-year)]
      (if(= synt nil)
        (str nimi)
          (str nimi " (" synt " - " kuol ")"))))

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (let [kirjat (apply str (interpose ", " (map book->string books)))]
    (if(empty? books)
      (str "No books.")
        (if(= (count books) 1)
          (str (count books) " book. "kirjat".")
         (str (count books) " books. "kirjat".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (str (author :name)) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
