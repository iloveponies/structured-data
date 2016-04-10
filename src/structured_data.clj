(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))

(defn spiff [v]
  (let [zeroth (get v 0)
        second (get v 2)]
        (+ zeroth second)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [{x 0 y 2} v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn lengths [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (point (- x2 x1) (- y2 y1))))

(defn width [rectangle]
  (let [{x 0} (lengths rectangle)]
    x))

(defn height [rectangle]
  (let [{y 1} (lengths rectangle)]
    y))

(defn square? [rectangle]
  (let [{x 0 y 1} (lengths rectangle)]
    (if (== (- x y) 0) true false)))

(defn area [rectangle]
  (let [{x 0 y 1} (lengths rectangle)]
    (* x y)))

(defn contains-point? [rectangle point]
  (let [[[x0 y0][x1 y1]] rectangle]
    (let [[x y] point]
    (if (and (<= x0 x x1) (<= y0 y y1)) true false))))

(defn contains-rectangle? [outer inner]
  (let [[[lowerX lowerY][upperX upperY]] inner]
    (let [lower (point lowerX lowerY)] 
      (let [upper (point upperX upperY)] 
        (if (and (contains-point? outer lower) (contains-point? outer upper)) true false)))))

(comment
(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
)
(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))
  (comment
  (let [authors (get-authors book)]
    (assoc authors new-author)
    authors)
  )

(defn get-authors [book]
  (get book :authors))

(defn alive? [author]
  (not (:death-year author)))
  ;(if (empty? (:death-year author)) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply >= a-seq) (apply <= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [length0 (count a-seq)]
    (let [length1 (count (set a-seq))]
      (if (== length0 length1) false true))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (:birth-year author)]
    (let [death (:death-year author)]
    (let [life (str (:name author))]
    (cond
      death (str life " (" (str birth) " - " (str death) ")")
      birth (str life " (" birth " - )")
      :else (str life))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [number (count books)]
    (let [book-list (apply str (interpose ". " (map book->string books)))]
      (cond
        (> number 1) (str number " books. " book-list ".")
        (== number 1) (str number " book. " book-list ".")
        :else (str "No books.")))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [name-found? 
        (fn [x]
          (if (= (:name x) name)
            true
            false))]
    (let [[result] (filter name-found? authors)]
      (if (= result ()) nil result))))

(defn is-nil? [x]
  (not x))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
