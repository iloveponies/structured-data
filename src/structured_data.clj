(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
       y (get v 2)]
  (+ y x)))

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
  (let [[[x1 y1][x2 y2]] rectangle
        p (- x2 x1)]
    (Math/abs p)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle
        p (- y2 y1)]
    (Math/abs p)))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
       [x3 y3] point]
    (if (and (>= x2 x3 x1) (>= y2 y3 y1))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[x y] inner]
    (if (and (contains-point? outer x) (contains-point? outer y))
      true
      false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (= (author-count book) 1)
    false
    true))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        addedauthors (conj authors new-author)]
    (assoc book :authors addedauthors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [elements] (get elements 1))]
     (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
    true
    (if (apply >= a-seq)
      true
      false)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [b-seq (set a-seq)]
    (if (= (count b-seq) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (let [old-authors (get book :authors)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (if (contains? (get book :authors) author)
    true
    false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [author]
  (let [aname (get author :name)
        abirth (get author :birth-year)
        adeath (get author :death-year)]
    (if adeath
      (str aname " (" abirth " - " adeath ")")
      (if abirth
        (str aname " (" abirth " - )")
        (str aname)))))


(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bname (str(get book :title))]
    (apply str bname ", written by " (authors->string (get book :authors)))))

(defn books->string [books]
  (let [num (count books)]
    (if (= num 0)
      "No books."
      (if (= num 1)
        (apply str (str num) " book. " (apply str(interpose ". " (map book->string books))) ".")
        (apply str (str num) " books. " (apply str(interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (= (count (living-authors (get book :authors))) 0)
    false
    true))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%



