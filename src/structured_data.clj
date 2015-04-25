(ns structured-data)


(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))


(defn spiff [v]
  (+ (get v 0) (get v 2)) )
  ;:-)


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))
  ;:-)

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
    (= (width rectangle) (height rectangle))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (width rectangle) (height rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (<= ox1 ix1 ix2 ox2) (<= oy1 iy1 iy2 oy2))))
  ;:-)

(defn title-length [book]
  (count (:title book)))
  ;:-)

(defn author-count [book]
  (count (:authors book)))
  ;:-)

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  ;(assoc book :authors new-author))
  (let [authors (:authors book)]
  (assoc book :authors (conj authors new-author))))
  ;:-)

(defn alive? [author]
  (not (:death-year author)))
  ;:-)

(defn element-lengths [collection]
  (map count (concat collection)))
  ;:-)

(defn second-elements [collection]
  (defn get2nd [v] (get v 1))
  (map get2nd collection))
  ;:-)

(defn titles [books]
  (map :title books))
  ;:-)

(defn monotonic? [a-seq]
  (let [[x y] a-seq]
  (cond
   (<= x y) (apply <= a-seq)
   (>= x y) (apply >= a-seq)
   :else false)))
  ;:-)

(defn stars [n]
  (apply str(repeat n "*")))
;  :-)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))
  ;:-)

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))
  ;:-)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))
  ;:-)

(defn has-author? [book author]
  (contains? (:authors book) author))
  ;:-)

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))
  ;:-)

(defn all-author-names [books]
  (set (map :name (authors books))))
  ;:-)

(defn author->string [author]
  (let [bj (:birth-year author) dj (:death-year author) n (:name author)]
  (if bj (str n " (" bj " - " dj ")") (str n))))
  ;(apply str author))
  ;:-)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))
  ;:-)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))
  ;:-)

(defn books->string [books]
  (let [n (count books)]
  (str (cond
   (= n 0) (str "No books")
   (= n 1) (str "1 book. " )
   :else (str n " books. " ))
  (str (apply str (interpose ". "  (map book->string books))) "." ))))
  ;:-)

(defn books-by-author [author books]
  (filter (fn[book] (has-author? book author)) books))

  ;:-)

(defn author-by-name [name authors]
  (let [found (filter (fn [author] (= (:name author) name)) authors)]
    (cond
     (> 1 (count found)) nil
     :else (first found))))
  ;:-)

(defn living-authors [authors]
  (filter (fn[author] (alive? author)) authors))
  ;:-)

(defn has-a-living-author? [book]
  (<= 1 (count (living-authors (:authors book)))))
  ;:-)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
  ;:-)

; %________%
