(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (cond
     (= (- x2 x1) (- y2 y1)) true
     :else false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (cond
     (and (<= ox1 ix1 ox2) (<= oy1 iy1 oy2)
          (<= ox1 ix2 ox2) (<= oy1 iy2 oy2)) true
          :else false
     )))


(defn title-length [book]
  (count (book :title)))

(defn author-count [book]
  (count (book :authors)))

(defn multiple-authors? [book]
  (cond
   (< 1 (author-count book)) true
   :else false))

(defn add-author [book new-author]
  (let [auth (get book :authors)]
    (assoc book :authors (conj auth new-author))) )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [vec2 (fn [vec]
               (get vec 1))]
       (map vec2 collection)))

(defn titles [books]
  (let [getTitle (fn [book]
                   (get book :title))]
    (map getTitle books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")) )

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))


(defn old-book->new-book [book]
 (let [auth (get book :authors)]
   (assoc book :authors (set auth))) )

(defn has-author? [book author]
  (let [auth (get book :authors)]
    (contains? auth author)))

(defn authors [books]
  (let [auth (map :authors books)]
   (set (apply clojure.set/union auth))))

(defn all-author-names [books]
  (let [names (map :name (authors books))]
    (set names)))

(defn author->string [author]
  (let [name (get author :name)
        bYear (get author :birth-year)
        dYear (get author :death-year)]
    (cond
     dYear (apply str [name " " "(" bYear " - " dYear ")"])
     bYear (apply str [name " " "(" bYear " - )"])
     :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
       auths (authors->string (get book :authors))]
    (apply str [title ", written by " auths])
    ))

(defn books->string [books]
  (let [strings (apply str (interpose ". " (map book->string books)))]
    (cond
     (empty? books) (str "No books.")
     (= (count books) 1)  (apply str [(count books) " book. " strings "."])
     :else (apply str [(count books) " books. " strings "."]))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [auth]
                   (if (= (:name auth) name)
                     true
                     nil)) authors)))

(defn living-authors [authors]
  (filter (fn [auth]
            (alive? auth)) authors))

(defn has-a-living-author? [book]
  (let [lAuth (get book :authors)]
    (not (empty? (living-authors lAuth)))))

(defn books-by-living-authors [books]
  (filter (fn [book]
            (has-a-living-author? book)) books))

; %________%
