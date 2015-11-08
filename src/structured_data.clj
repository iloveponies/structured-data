(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)
    ))

(defn spiff [v]
  (if (> (count v) 2) (+ (get v 0) (get v 2)) nil)
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v1 _ v3]]
  (+ v1 v3))

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
  (= (- y2 y1) (- x2 x1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
   (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (<= x1 ix1 ix2 x2) (<= y1 iy1 iy2 y2)) ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn[x] (count x)) collection))

(defn second-elements [collection]
  (map (fn[x] (get x 1)) collection))

(defn titles [books]
  (map (fn[b] (:title b)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)  (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author) (if (contains? author :birth-year)
     (apply str (concat [" (" (:birth-year author) " - " (:death-year author)  ")"]))
    nil)))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))
  ))

(defn books->string [books]
  (if (= (count books) 0) "No books."
  (str (count books) " book" (if (> (count books) 1) "s" nil) ". "
       (clojure.string/join ". " (map book->string books))
  ".")))

(defn books-by-author [author books]
  (filter (fn[x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn[x] (= name (:name x)) ) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
