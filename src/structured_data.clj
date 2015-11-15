(ns structured-data)

(defn do-a-thing [x]
  (let [n x] (Math/pow (+ x x) (+ x x))))

(defn spiff [v]
  (+ (get v 0) (get v 2) ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v1 v2 v3]]
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
    (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
    (* (width rectangle) (height rectangle))) 

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= y1 y3 y2) (<= x1 x3 x2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (if (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4])) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [ add (fn [bk au] (assoc bk :authors (conj (:authors bk) au)))]
    (add book new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (let [countel (fn [x] (count x))]
    (map countel collection)))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
  (let [ttl (fn [x] (get x :title))]
    (map ttl books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (let [conv (fn [bk] (assoc book :authors (set (:authors bk))))]
    (conv book)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [au (fn [x] (:authors x))]
    (apply clojure.set/union (map au books))))

(defn all-author-names [books]
  (let [nm (fn [x] (get x :name))]
    (set (map nm (authors books)))))

(defn author->string [author]
  (let [yr (fn [x] (if (:death-year x)
                     (str " (" (:birth-year x) " - " (:death-year x) ")")
                     (if (:birth-year x)
                       (str " (" (:birth-year x) " - )")
                       nil)))]
    (str (:name author) (yr author))))

(defn authors->string [authors]
  (let [st (fn [x] (map author->string x))]
    (apply str (interpose ", " (st authors)))))

(defn book->string [book]
  (let [au (fn [x] (authors->string (:authors x)))]
    (str (:title book) ", written by " (au book))))

(defn books->string [books]
  (let [bs (apply str (interpose ", " (map book->string books)))]
    (cond
      (== (count books) 0) (str "No books.")
      (== (count books) 1) (str "1 book. " bs ".")
      :else (str (count books) " books. " bs "."))))

(defn books-by-author [author books]
  (let [bk (fn [x] (has-author? x author))] 
    (filter bk books)))

(defn author-by-name [name authors]
  (let [nm (fn [x] (= name (get x :name)))]
    (first (filter nm authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
