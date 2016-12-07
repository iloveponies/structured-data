(ns structured-data)

(defn do-a-thing [x]
  (let [name (+ x x)]
  (Math/pow name name)))

(do-a-thing 2)

(defn spiff [v]
  (let [eka (get v 0) toka (get v 2)]
    (if (or (nil? eka) (nil? toka))
      nil
      (+ eka toka))))

(spiff [1 2 3])       ;=> 4
(spiff [1 2 3 4 5 6]) ;=> 4
(spiff [1 2])         ;=> ?
(spiff [])            ;=> ?

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[eka toka] [(get v 0) (get v 2)]]
    (+ eka toka)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [tulos (- x1 x2)]
      (if (< tulos 0)
        (* -1 tulos)
        tulos))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [tulos (- y1 y2)]
      (if (< tulos 0)
        (* -1 tulos)
        tulos))))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn abs [x]
  (if (< x 0)
    (* x -1)
    x))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (cond
        (and (<= x1 px x2) (<= y1 py y2)) true
        (and (<= x2 px x1) (<= y2 py y1)) true
        (and (<= x1 px x2) (<= y2 py y1)) true
        (and (<= x2 px x1) (<= y1 py y2)) true
        :else false))))

(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point 2 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))           ;=> false
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))            ;=> false
(contains-point? (rectangle [1 1] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [1 1] [1 1])
                 (point 1 1))            ;=> true

(defn contains-rectangle? [outer inner]
  (let [[ip1 ip2] inner]
    (if (and (contains-point? outer ip1) (contains-point? outer ip2))
      true
      false)))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false



(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))


(defn alive? [author]
  (if (nil? (:death-year author))
    true
    false))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [temp (fn [col] (get col 1))]
    (map temp collection)))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(stars 5)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq))
    true
    false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [n (:name author)
        b (:birth-year author)
        d (:death-year author)]
    (if (nil? b)
      (str n)
      (str n " (" b " - " d ")"))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [t (:title book)
        a (authors->string (:authors book))]
    (str t ", written by " a)))

(defn books->string [books]
  (let [c (count books)
       start (cond
                (= c 0) "No books."
                (= c 1) "1 book. "
                :else (str c " books. "))
        end (cond
              (= c 0) ""
              :else ".")]
    (str start (apply str (interpose ", " (map book->string books))) end)))

(defn books-by-author [author books]
  (let [curr (:book books)]
    (filter (fn [curr] (has-author? curr author)) books)))


(defn author-by-name [name authors]
  (let [author (:author authors)]
      (first (filter (fn [author] (= (:name author) name)) authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (if (empty? (living-authors authors))
      false
      true)))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
