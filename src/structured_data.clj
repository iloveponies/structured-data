(ns structured-data)

(defn do-a-thing [x]
  (let [asd (+ x x)]
    (Math/pow asd asd)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (and (<= x1 x x2)
           (<= y1 y y2)))))

(defn contains-rectangle? [outer inner]
  (let[[x y] inner]
    (and (contains-point? outer x)
         (contains-point? outer y))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [asd (fn [x] (get x 1))]
    (map asd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (let [athrs (authors books)]
    (set (map :name athrs))))

(defn author->string [author]
  (let [n (str (get author :name))]
    (let [b (get author :birth-year)]
      (let [d (get author :death-year)]
        (if (contains? author :birth-year)
          (str n " (" b " - " d ")")
          (str n)
          )))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)]
    (let [authors (authors->string (get book :authors))]
      (str title ", written by " authors))))

(defn books->string [books]
  (let [i (count books)]
    (if (== i 0)
      "No books."
      (if (== i 1)
        (str "1 book. " (apply book->string books) ".")
        (str i " books. " (apply str (interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
  (filter (fn[x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (x :name) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [a (book :authors)]
    (> (count (filter alive? a)) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
