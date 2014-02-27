(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
 (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c)))

(defn point [x y] [x y])

(defn rectangle [bottom-left top-right]  [bottom-left top-right])

(defn width [rectangle]
 (let [[[x1 y1][x2 y2]] rectangle]
   (- x2 x1)))

(defn height [rectangle]
   (let [[[x1 y1][x2 y2]] rectangle ]
     (- y2 y1)))

(defn square? [rectangle]
  (= 0 (- (width rectangle) (height rectangle))))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [xx1 yy1] point]
    (and (<= x1 xx1 x2) (<= y1 yy1 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] outer
        [[xx1 yy1][xx2 yy2]] inner]
     (and (<= x1 xx1 xx2 x2) (<= y1 yy1 yy2 y2))
 ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count(get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn getsecond [s] (get s 1))

(defn second-elements [collection]
  (map getsecond collection))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [xs x]
  (let [ys (set xs)]
    (cond
     (contains? ys x) (disj ys x)
     :else (conj ys x))))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))


(defn old-book->new-book [book]
  (assoc book :title (get book :title) :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author)
  )

(defn getauthors [abook]
  (get abook :authors))

(defn authors [books]
  (apply clojure.set/union (map getauthors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [a]
    (let [dy (get a :death-year)
        by (get a :birth-year)
        nm (get a :name)]

    (cond
     dy (str nm " (" by " - " dy ")")
     by (str nm " (" by " - )")
     :else (str nm)
     )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)]
    (cond
     (= 0 n) (str "No books.")
     (= 1 n) (str "1 book. " (book->string (first books)) ".")
     :else (str n " books. " (apply str
                                    (interpose ". " (map book->string books))) ".")
     )
    )
  )

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn find-author [a auth]
             ( = (:name auth) a))

(defn author-by-name [name authors]
 (first (filter (comp (partial = name) :name) authors)))

(defn living-authors [authors]
   (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
 (filter has-a-living-author? books))

; %________%
