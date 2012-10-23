    (ns structured-data)

    (defn do-a-thing [x]
      (let [xx (+ x x)]
      (Math/pow xx xx)))

    (defn spiff [v]
      (if(> (count v) 2)
        (+ (get v 0) (get v 2))
        nil))

    (defn cutify [v]
      (conj v "<3"))

    (defn spiff-destructuring [v]
      (let[[x y z] v]
      (+ x z)))

    (defn point [x y]
      [x y])

    (defn rectangle [bottom-left top-right]
      [bottom-left top-right])

    (defn width [rectangle]
      (let [[x y] rectangle]
        (- (get y 0) (get x 0))))

    (defn height [rectangle]
      (let [[x y] rectangle]
        (- (get y 1) (get x 1))))

    (defn square? [rectangle]
      (if(= (height rectangle)(width rectangle))
        true
        false))

    (defn area [rectangle]
      (* (height rectangle) (width rectangle)))

    (defn contains-point? [rectangle point]
      (let [[x y] rectangle]
        (if (and (<= (get x 0) (get point 0) (get y 0)) (<= (get x 1) (get point 1) (get y 1)))
          true
          false)))

    (defn contains-rectangle? [outer inner]
      (let[[x y] inner]
        (if (contains-point? outer x)
          (if (contains-point? outer y)
            true
            false)
          false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj(:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
   (let [elemental (fn [x] (get x 1))]
    (map elemental collection)))

(defn titles [books]
  (map :title books ))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
     true
     false))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
     (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [s (set a-seq)]
    (if (== (count s) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (if (contains? (book :authors) author)
    true
    false))

(defn authors [books]
   (apply clojure.set/union [map :authors books]))

(defn all-author-names [books]
  (apply clojure.set/union [map name (:authors books)]))

(defn author->string [author]
  (let [name (:name author)
        b (:birth-year author)
        d (:death-year author)]
   str(name (b - d))))

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)
