(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (let [first-el (get v 0)
        second-el (get v 2)]
    (+  first-el second-el)))

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
    (int (Math/sqrt (Math/pow (- x2 x1) 2)))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (int (Math/sqrt (Math/pow (- y2 y1) 2)))))

(defn square? [rectangle]
  (let [width (width rectangle)
        height (height rectangle)]
    (if (= width height)
      true
      false)))

(defn area [rectangle]
  (let [width (width rectangle)
        height (height rectangle)]
    (* width height)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and (<= x1 xp x2) (<= y1 yp y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1) (contains-point? outer p2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
    (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [old-authors (:authors book)]
        (assoc book :authors (conj old-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [munge (fn [x] (get x 1))]
    (seq (map munge collection))))

(defn titles [books]
  (let [munge (fn [x] (:title x))]
    (seq (map munge books))))

(defn monotonic? [a-seq]
  (if (or
        (apply <= a-seq)
        (apply >= a-seq))
        true
        false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (not (= (count a-seq) (count a-set)))
      true
      false)))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
    (contains? author :death-year) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year) (str (:name author) " (" (:birth-year author) " - " ")")
    :else (str (:name author))
    ))

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [name-of-book (:title book)
        authors-of-book (:authors book)
        string-authors (authors->string authors-of-book)]
    (apply str (interpose ", written by " (vector name-of-book string-authors)))))

(defn books->string [books]
  (let [a-set-books (set books)
        count-of-books (count a-set-books)
        txt-info (fn [x]
                   (cond
                     (= x 0) "No books"
                     (= x 1) "1 book. "
                     :else (str x " books. ")))]

  (str (apply str (txt-info count-of-books) (interpose ". " (map book->string books))) ".")
    ))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [name-of-author (filter (fn [x] (= name (:name x))) authors)]
    (if (empty? name-of-author)
      nil
      (first name-of-author))))


(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
   (let [alive-authors (living-authors (:authors book))]
    (if (not (empty? alive-authors))
      true
      false)))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
