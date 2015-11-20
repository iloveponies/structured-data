(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (cond
   (>= (count v) 3) (+ (get v 0) (get v 2))
  :else nil
   ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (cond (>= (count v) 3)
        (let [[x y z] v] (+ x z))
   :else nil))

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
  (let [[[x1 y1][x2 y2]] rectangle
        [xp yp] point]
    (and (and (<= xp x2) (>= xp x1)) (and (<= yp y2) (>= yp y1)))
    ))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p2) (contains-point? outer p1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [ new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection)
)

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map (fn [b] (:title b)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [x] (:authors x)) books)))

(defn all-author-names [books]
  (set (map (fn [x] (:name x)) (authors books))))

(defn author->string [author]
  (let [
        pre (:name author)
        post (cond
              (contains? author :death-year) (str " (" (str (:birth-year author)) " - " (str(:death-year author)) ")")
              (contains? author :birth-year) (str " (" (str(:birth-year author)) " - )")
              :else "")
        ]
    (str pre post)))

(defn authors->string [authors]
  (apply str (interpose ", " (map (fn [x] (author->string x)) authors))))

(defn book->string [book]
  (let [bt (:title book)
        mt ", written by "
        et (authors->string (:authors book))]
    (str bt mt et)))

(defn books->string [books]
  (let [ini (cond
   (empty? books) "No books."
   (= (count books) 1) "1 book."
   :else (str (count books) " books."))]
    (str ini (apply str (map (fn [b] (str " " (book->string b) ".")) books)))
    ))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        living_authors (filter alive? authors)]
    (> (count living_authors) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
