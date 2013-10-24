(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (if
    (== (width rectangle)(height rectangle))
    true
    false))

(defn area [rectangle]
    (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [pointx pointy] point]
    (if
      (and (>= pointx x1) (>= pointy y1) (<= pointx x2) (<= pointy y2))
      true
      false)))


(defn contains-rectangle? [outer inner]
  (let [[[outerx1 outery1] [outerx2 outery2]] outer
        [[innerx1 innery1] [innerx2 innery2]] inner]
    (if
      (and (>= innerx1 outerx1) (>= innery1 outery1) (<= innerx2 outerx2) (<= innery2 outery2))
      true
      false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if
    (> (count (get book :authors)) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        x (conj authors new-author)]
    (assoc book :authors x)
    ))

(defn alive? [author]
    (= nil (get author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements[collection]
  (let [snd (fn [collection] (get collection 1))]
    (map snd collection))
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply >= a-seq) true
   (apply <= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [sequ]
  (not (== (count sequ) (count (set sequ)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
    (let [neim (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if
      (= nil birth)
      neim
      (if
        (= nil death)
        (str neim " (" birth " - )")
        (str neim " (" birth " - " death ")")))))


(defn authors->string [authors]
  (apply str ( interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn mongobongo [books]
  (apply str (interpose ". " (map book->string books))))

(defn books->string [books]
  (let [kount (count books)]
    (cond
      (> kount 1) (str kount " books. " (mongobongo books) ".")
      (== kount 1)(str kount " book. " (mongobongo books) ".")
      (== kount 0) "No books.")))


(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [name authors]
   (let [author (filter (fn [x] (= (:name x) name)) authors)]
     (if
       (== (count author) 1)
       (first author)
       nil)))

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
    (> (count (living-authors (:authors book))) 0)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

;
