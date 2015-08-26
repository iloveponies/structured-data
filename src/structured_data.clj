(ns structured-data)

(defn do-a-thing [x]
  (let [tupla (+ x x)] (Math/pow tupla tupla)))

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
    (defn abs [x]
      (if (< x 0) (- x) x))
         (abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (defn abs [x]
      (if (< x 0) (- x) x))
         (abs (- y1 y2))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (if (and (<= x1 x x2) (<= y1 y y2)) true false))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[x3 y3] [x4 y4]] inner]
      (if (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4])) true false))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (>= (author-count book) 2) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [toiset (fn [x] (get x 1))]
    (map toiset collection)))

(defn titles [books]
  (let [nimet (fn [x] (:title x))]
  (map nimet books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [bset (set a-seq)]
    (if (= (count a-seq) (count bset)) false true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [teki (fn [x] (:authors x))]
  (apply clojure.set/union (map teki books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author)]
    (let [svuosi (:birth-year author)]
      (let [kvuosi (:death-year author)]
        (str nimi (if (not (nil? svuosi)) (str " (" svuosi " - " kvuosi ")")))))))

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [luku (count books)]
    (if (= luku 0) "No books."
      (str luku (if (= luku 1) " book. " " books. ")
             (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [apu (fn [book] (has-author? book author))]
   (filter apu books)))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

